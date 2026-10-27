;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.contexts
  "The token contexts in the tokens sidebar, a sticky footer of the sets
  panel: the page's scope and every board's. Clicking a context gives
  the selection its themes; hovering one highlights its board; right
  clicking opens a menu to pick its themes."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.tokens.scopes :as dwts]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*] :as i]
   [app.main.ui.hooks :as hooks]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn- current-context
  "The context the selection resolves with: the id of the nearest board
  with a scope, :page, or nil outside every context."
  [page selected]
  (let [shape-id (when (= 1 (count selected)) (first selected))]
    (some-> (cfo/scope-chain page (:objects page) shape-id) peek :origin)))

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

(mf/defc theme-items*
  "Every theme as \"Group / Theme\", checked when `active?` says so.
  Clicking one calls `on-toggle` with it and whether it was active."
  {::mf/private true}
  [{:keys [tokens-lib active? on-toggle]}]
  [:*
   (for [[_ themes] (ctob/get-theme-tree-no-hidden tokens-lib)
         [_ theme]  themes
         :let [checked? (active? (:id theme))]]
     [:> menu-item* {:key (dm/str (:id theme))
                     :label (dwts/theme-path theme)
                     :is-checked checked?
                     :on-click #(on-toggle theme checked?)}])])

(mf/defc scope-menu*
  "The themes of a page's or a board's scope: checking a theme sets it
  for its group, unchecking it leaves the group to the parent."
  {::mf/private true}
  [{:keys [tokens-lib scope on-change on-remove]}]
  (let [themes (:themes scope)]
    [:*
     [:> theme-items* {:tokens-lib tokens-lib
                       :active? #(contains? themes %)
                       :on-toggle (fn [theme checked?]
                                    (on-change (dwts/set-group-theme scope tokens-lib (:group theme)
                                                                     (when-not checked? (:id theme)))))}]
     [:li {:class (stl/css :menu-separator)}]
     [:li {:class (stl/css :menu-item)
           :role "menuitem"
           :on-click on-remove}
      (tr "workspace.tokens.contexts.remove")]]))

(mf/defc context-menu*
  {::mf/private true}
  [{:keys [menu page tokens-lib on-close]}]
  (let [container (hooks/use-portal-container :popup)
        {:keys [position target]} menu
        scope     (if (= target :page)
                    (:token-scope page)
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
         (when scope
           [:> scope-menu* {:tokens-lib tokens-lib
                            :scope scope
                            :on-change on-change
                            :on-remove #(on-change nil)}])]]])
     container)))

;; List ----------------------------------------------------------------------

(mf/defc context-row*
  {::mf/private true}
  [{:keys [id name label is-current on-open-menu]}]
  (let [board-id (when (uuid? id) id)

        ;; With a selection: give the selected boards this context
        on-click
        (mf/use-fn
         (mf/deps id)
         #(st/emit! (dwts/assign-token-scope id)))

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
  "The contexts. A child of the sets panel, which scrolls: the section
  sticks to its bottom."
  []
  (let [tokens-lib    (mf/use-ctx ctx/tokens-lib)
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
        (mf/use-fn #(reset! menu* nil))

        can-edit?
        (mf/use-ctx ctx/can-edit?)

        ;; The selected boards get an empty context, or the page with no
        ;; board selected
        on-add
        (mf/use-fn #(st/emit! (dwts/add-token-scope)))]

    (when (and tokens-lib page)
      [:*
       [:div {:class (stl/css :contexts-section)}
        [:div {:class (stl/css :contexts-header)}
         [:> title-bar* {:title (tr "workspace.tokens.contexts.title")}
          (when can-edit?
            [:> icon-button* {:variant "ghost"
                              :icon i/add
                              :aria-label (tr "workspace.tokens.contexts.add")
                              :on-click on-add}])]]
        [:div {:class (stl/css :contexts-list)}
         (for [{:keys [id name scope]} items]
           [:> context-row* {:key (dm/str id)
                             :id id
                             :name name
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
                            :on-close on-close-menu}])])))
