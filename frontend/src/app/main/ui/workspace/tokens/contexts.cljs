;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.contexts
  "The token contexts in the tokens sidebar, a sticky footer of the sets
  panel: the global context (the file's themes), the page's scope and
  every board's. Clicking a context gives the selection its themes;
  hovering one highlights its board; right clicking opens a menu to pick
  its themes."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.common.types.tokens-status :as ctos]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.tokens.library-edit :as dwtl]
   [app.main.data.workspace.tokens.scopes :as dwts]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*] :as i]
   [app.main.ui.hooks :as hooks]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn- current-context
  "The context the selection resolves with: the id of the nearest board
  with a scope, :page, or :global."
  [page selected]
  (let [shape-id (when (= 1 (count selected)) (first selected))]
    (or (some-> (cfo/scope-chain page (:objects page) shape-id) peek :origin)
        :global)))

(defn- context-items
  "The page's context, if it has a scope, then the boards with one, by name."
  [page]
  (cond-> []
    (:token-scope page)
    (conj {:id :page :name (:name page) :scope (:token-scope page)})

    :always
    (into (comp (filter #(and (:token-scope %) (dwts/scope-holder? %)))
                (map (fn [shape] {:id (:id shape) :name (:name shape) :scope (:token-scope shape)})))
          (sort-by :name (vals (:objects page))))))

(defn- file-themes
  [tokens-status tokens-lib]
  (remove ctob/hidden-theme? (cfo/get-active-themes tokens-status tokens-lib)))

;; Menu ----------------------------------------------------------------------

(mf/defc menu-item*
  {::mf/private true}
  [{:keys [label is-checked on-click]}]
  [:li {:class (stl/css :menu-item)
        :role "menuitemcheckbox"
        :aria-checked (boolean is-checked)
        :on-click on-click}
   [:span {:class (stl/css :menu-item-label)} label]
   [:> icon* {:icon-id i/tick
              :size "s"
              :class (stl/css-case :menu-check true
                                   :menu-check-visible is-checked)}]])

(mf/defc group-items*
  "One theme group: the entry for no theme of the group, then its themes.
  `selected?` tells the checked themes, `on-pick` gets the theme id, or
  nil for the first entry."
  {::mf/private true}
  [{:keys [group themes none-label selected? on-pick]}]
  (let [none? (not-any? #(selected? (:id %)) themes)]
    [:*
     [:li {:class (stl/css :menu-group)}
      (if (seq group) group (tr "workspace.tokens.contexts.ungrouped"))]
     [:> menu-item* {:label none-label
                     :is-checked none?
                     :on-click #(on-pick nil)}]
     (for [theme themes]
       [:> menu-item* {:key (dm/str (:id theme))
                       :label (:name theme)
                       :is-checked (selected? (:id theme))
                       :on-click #(on-pick (:id theme))}])]))

(defn- theme-groups
  [tokens-lib]
  (for [[group themes] (ctob/get-theme-tree-no-hidden tokens-lib)]
    [group (vals themes)]))

(mf/defc scope-menu*
  "The themes of a page's or a board's scope."
  {::mf/private true}
  [{:keys [tokens-lib scope on-change on-remove]}]
  (let [isolated? (= :isolated (:mode scope))
        themes    (:themes scope)]
    [:*
     [:> menu-item* {:label (tr "workspace.tokens.contexts.inherit")
                     :is-checked (not isolated?)
                     :on-click #(on-change (dwts/set-isolated scope (not isolated?)))}]
     (for [[group group-themes] (theme-groups tokens-lib)]
       [:> group-items* {:key group
                         :group group
                         :themes group-themes
                         :none-label (if isolated?
                                       (tr "workspace.tokens.contexts.group-none")
                                       (tr "workspace.tokens.contexts.group-inherit"))
                         :selected? #(contains? themes %)
                         :on-pick #(on-change (dwts/set-group-theme scope tokens-lib group %))}])
     [:li {:class (stl/css :menu-separator)}]
     [:li {:class (stl/css :menu-item)
           :role "menuitem"
           :on-click on-remove}
      (tr "workspace.tokens.contexts.remove")]]))

(mf/defc global-menu*
  "The file's themes: picking one activates it in place of the other
  themes of its group."
  {::mf/private true}
  [{:keys [tokens-lib tokens-status]}]
  [:*
   (for [[group group-themes] (theme-groups tokens-lib)]
     (let [active? #(ctos/theme-active? tokens-status %)]
       [:> group-items* {:key group
                         :group group
                         :themes group-themes
                         :none-label (tr "workspace.tokens.contexts.group-none")
                         :selected? active?
                         :on-pick (fn [theme-id]
                                    (cond
                                      (nil? theme-id)
                                      (apply st/emit! (for [theme group-themes
                                                            :when (active? (:id theme))]
                                                        (dwtl/toggle-token-theme-active (:id theme))))

                                      (not (active? theme-id))
                                      (st/emit! (dwtl/toggle-token-theme-active theme-id))))}]))])

(mf/defc context-menu*
  {::mf/private true}
  [{:keys [menu page tokens-lib tokens-status on-close]}]
  (let [container (hooks/use-portal-container :popup)
        {:keys [position target]} menu
        scope     (case target
                    :global nil
                    :page   (:token-scope page)
                    (get-in page [:objects target :token-scope]))
        on-change (fn [scope']
                    (st/emit! (if (= target :page)
                                (dwts/set-page-token-scope scope')
                                (dwts/set-shape-token-scope target scope'))))]
    (mf/portal
     (mf/html
      [:div {:class (stl/css :menu-portal)
             :style {:top (:y position) :left (:x position)}
             :on-context-menu dom/prevent-default}
       [:& dropdown {:show true :on-close on-close}
        [:ul {:class (stl/css :menu) :role "menu"}
         (if (= target :global)
           [:> global-menu* {:tokens-lib tokens-lib
                             :tokens-status tokens-status}]
           (when scope
             [:> scope-menu* {:tokens-lib tokens-lib
                              :scope scope
                              :on-change on-change
                              :on-remove #(on-change nil)}]))]]])
     container)))

;; List ----------------------------------------------------------------------

(mf/defc context-row*
  {::mf/private true}
  [{:keys [id name label scope is-current on-open-menu]}]
  (let [board-id (when (uuid? id) id)

        ;; With a selection: give the selected boards this context
        on-click
        (mf/use-fn
         (mf/deps scope)
         #(st/emit! (dwts/assign-token-scope scope)))

        on-pointer-enter
        (mf/use-fn
         (mf/deps board-id)
         #(when board-id (st/emit! (dw/highlight-shape board-id))))

        on-pointer-leave
        (mf/use-fn
         (mf/deps board-id)
         #(when board-id (st/emit! (dw/dehighlight-shape board-id))))

        on-context-menu
        (mf/use-fn
         (mf/deps id on-open-menu)
         (fn [event]
           (dom/prevent-default event)
           (dom/stop-propagation event)
           (on-open-menu id (dom/get-client-position event))))]
    [:div {:class (stl/css-case :context-row true
                                :current-context is-current)
           :role "button"
           :title label
           :on-click on-click
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave
           :on-context-menu on-context-menu}
     [:span {:class (stl/css :context-name)} name]
     [:span {:class (stl/css :context-label)} label]]))

(mf/defc contexts-section*
  "The contexts, the global one first. A child of the sets panel, which
  scrolls: the section sticks to its bottom."
  []
  (let [tokens-lib    (mf/use-ctx ctx/tokens-lib)
        tokens-status (mf/use-ctx ctx/tokens-status)
        page          (mf/deref refs/workspace-page)
        selected      (mf/deref refs/selected-shapes)

        items   (mf/with-memo [page] (context-items page))
        current (mf/with-memo [page selected] (current-context page selected))

        menu*   (mf/use-state nil)
        menu    (deref menu*)

        on-open-menu
        (mf/use-fn
         (fn [target position]
           (reset! menu* {:target target :position position})))

        on-close-menu
        (mf/use-fn #(reset! menu* nil))]

    (when (and tokens-lib tokens-status page)
      [:*
       [:div {:class (stl/css :contexts-section)}
        [:div {:class (stl/css :contexts-header)}
         [:> title-bar* {:title (tr "workspace.tokens.contexts.title")}]]
        [:div {:class (stl/css :contexts-list)}
         [:> context-row* {:id :global
                           :name (tr "workspace.tokens.contexts.global")
                           :label (dwts/themes-label tokens-lib (file-themes tokens-status tokens-lib))
                           :is-current (= :global current)
                           :on-open-menu on-open-menu}]
         (for [{:keys [id name scope]} items]
           [:> context-row* {:key (dm/str id)
                             :id id
                             :name name
                             :scope scope
                             :label (dwts/scope-label tokens-lib scope)
                             :is-current (= id current)
                             :on-open-menu on-open-menu}])
         (when (empty? items)
           [:div {:class (stl/css :contexts-empty)}
            (tr "workspace.tokens.contexts.empty")])]]

       (when menu
         [:> context-menu* {:menu menu
                            :page page
                            :tokens-lib tokens-lib
                            :tokens-status tokens-status
                            :on-close on-close-menu}])])))
