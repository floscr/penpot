;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.token-scope
  "The token themes section of the design panel: the themes a page or a
  board resolves its tokens with (see `cto/schema:token-scope`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.workspace.tokens.scopes :as dwts]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.i18n :refer [tr]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

;; The first option of every group: the group is not set by this scope.
;; Theme ids are uuids, so the id cannot collide with one; the select
;; shows a blank id as no selection.
(def ^:private inherit-option-id "inherit")

(defn- origin-label
  [origin objects]
  (case origin
    :file (tr "workspace.options.token-scope.origin-file")
    :page (tr "workspace.options.token-scope.origin-page")
    (get-in objects [origin :name])))

(defn- inherit-label
  "What a group resolves with when this scope does not name one of its
  themes: the inherited theme and where it comes from, or nothing."
  [isolated? inherited tokens-lib objects]
  (cond
    isolated?
    (tr "workspace.options.token-scope.none")

    inherited
    (tr "workspace.options.token-scope.inherited"
        (:name (ctob/get-theme tokens-lib (:theme-id inherited)))
        (origin-label (:origin inherited) objects))

    :else
    (tr "workspace.options.token-scope.inherited-none")))

(defn- normalize-scope
  "A cascade scope without themes is no scope."
  [{:keys [themes mode] :as scope}]
  (when (or (seq themes) (= mode :isolated))
    (cond-> scope
      (not= mode :isolated) (dissoc :mode))))

(defn- set-group-theme
  "The scope with `theme` as the theme of `group`, or without a theme of
  that group when `theme` is nil."
  [scope tokens-lib group theme]
  (let [in-group? (fn [id] (= group (:group (ctob/get-theme tokens-lib id))))
        themes    (cond-> (into #{} (remove in-group?) (:themes scope))
                    theme (conj (:id theme)))]
    (normalize-scope (assoc scope :themes themes))))

(mf/defc token-scope-menu*
  [{:keys [scope chain-above on-change]}]
  (let [tokens-lib    (mf/deref refs/tokens-lib)
        tokens-status (mf/deref refs/tokens-status)
        objects       (mf/deref refs/workspace-page-objects)

        groups
        (mf/with-memo [tokens-lib]
          (when tokens-lib
            (ctob/get-theme-tree-no-hidden tokens-lib)))

        themes-by-option-id
        (mf/with-memo [tokens-lib]
          (into {}
                (map (fn [theme] [(str (:id theme)) theme]))
                (when tokens-lib (ctob/get-themes tokens-lib))))

        inherited
        (mf/with-memo [tokens-status tokens-lib chain-above]
          (when (and tokens-status tokens-lib)
            (cfo/theme-origin-by-group tokens-status tokens-lib chain-above)))

        isolated? (= :isolated (:mode scope))
        themes    (or (:themes scope) #{})

        on-toggle-inherit
        (mf/use-fn
         (mf/deps scope on-change)
         (fn [inherit?]
           (on-change (normalize-scope (assoc scope
                                              :themes (or (:themes scope) #{})
                                              :mode (if inherit? :cascade :isolated))))))

        on-select
        (mf/use-fn
         (mf/deps scope tokens-lib themes-by-option-id on-change)
         (fn [group option-id]
           (on-change (set-group-theme scope tokens-lib group (get themes-by-option-id option-id)))))

        on-reset
        (mf/use-fn
         (mf/deps on-change)
         #(on-change nil))]

    (when (seq groups)
      [:section {:class (stl/css :element-set)
                 :aria-label (tr "workspace.options.token-scope.title")}
       [:div {:class (stl/css :element-title)}
        [:> title-bar* {:collapsable false
                        :title (tr "workspace.options.token-scope.title")}
         (when scope
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.options.token-scope.reset")
                             :on-click on-reset
                             :icon i/remove}])]]

       [:div {:class (stl/css :element-content)}
        [:div {:class (stl/css :row)}
         [:span {:class (stl/css :row-label :row-label-wide)}
          (tr "workspace.options.token-scope.inherit")]
         [:div {:class (stl/css :row-switch)}
          [:> switch* {:key (str "inherit-" isolated?)
                       :default-checked (not isolated?)
                       :aria-label (tr "workspace.options.token-scope.inherit")
                       :on-change on-toggle-inherit}]]]

        (for [[group group-themes] groups
              :let [selected (some (fn [[_ theme]] (when (contains? themes (:id theme)) theme))
                                   group-themes)
                    selected-id (if selected (str (:id selected)) inherit-option-id)
                    label (if (seq group) group (tr "workspace.options.token-scope.ungrouped"))
                    options (into [{:id inherit-option-id
                                    :label (inherit-label isolated? (get inherited group) tokens-lib objects)}]
                                  (map (fn [[_ theme]] {:id (str (:id theme)) :label (:name theme)}))
                                  group-themes)]]
          [:div {:key group
                 :class (stl/css :row)}
           [:span {:class (stl/css :row-label)
                   :title label}
            label]
           [:div {:class (stl/css :row-select)}
            [:> select* {:key (str group "-" selected-id)
                         :default-selected selected-id
                         :options options
                         :aria-label label
                         :on-change (partial on-select group)}]]])]])))

(mf/defc page-token-scope-menu*
  "The token themes section of the current page."
  []
  (let [scope-ref (mf/with-memo []
                    (l/derived :token-scope refs/workspace-page))
        scope     (mf/deref scope-ref)
        on-change (mf/use-fn #(st/emit! (dwts/set-page-token-scope %)))]
    [:> token-scope-menu* {:scope scope
                           :chain-above []
                           :on-change on-change}]))

(mf/defc shape-token-scope-menu*
  "The token themes section of a board."
  [{:keys [shape]}]
  (let [shape-id   (:id shape)
        parent-id  (:parent-id shape)
        scope-ref  (mf/with-memo []
                     (l/derived :token-scope refs/workspace-page))
        page-scope (mf/deref scope-ref)
        objects    (mf/deref refs/workspace-page-objects)

        chain-above
        (mf/with-memo [page-scope objects parent-id]
          (cfo/scope-chain {:token-scope page-scope} objects parent-id))

        on-change
        (mf/use-fn
         (mf/deps shape-id)
         #(st/emit! (dwts/set-shape-token-scope shape-id %)))]
    [:> token-scope-menu* {:scope (:token-scope shape)
                           :chain-above chain-above
                           :on-change on-change}]))
