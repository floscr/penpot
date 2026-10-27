;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.viewport.token-scopes
  "Outlines of the token scopes on the canvas: every shape with a
  `:token-scope` gets a purple outline and a label with its themes, and a
  page with a scope a frame around the viewport."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.common.types.tokens-lib :as ctob]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.refs :as refs]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn visible?
  "Whether the scope outlines show: with the flag, while the tokens tab of
  the left sidebar is open. `layout` keeps :tokens when another tab is
  picked, so the tabs that win over it in the sidebar rule it out here."
  [layout options-mode]
  (and (contains? cf/flags :token-scoped-themes)
       (contains? layout :tokens)
       (not (contains? layout :layers))
       (not (contains? layout :assets))
       (not= options-mode :inspect)))

(defn- scope-label
  "The names of the scope's themes in theme group order, and a mark when
  the scope is isolated."
  [tokens-lib {:keys [themes mode]}]
  (let [themes (keep #(ctob/get-theme tokens-lib %) themes)
        order  (into {} (map-indexed (fn [i group] [group i])) (ctob/get-theme-groups tokens-lib))
        names  (->> themes
                    (sort-by #(get order (:group %) -1))
                    (map :name)
                    (str/join " · "))
        names  (if (str/blank? names) (tr "workspace.token-scope-area.no-themes") names)]
    (if (= mode :isolated)
      (tr "workspace.token-scope-area.isolated" names)
      names)))

;; Width of a badge's box in screen pixels; the label is cut to it.
(def ^:private badge-width 400)

(mf/defc scope-badge*
  "A label drawn at the canvas point `x`, `y`, at the same size on screen
  whatever the zoom. With `is-end` the label ends at `x`."
  [{:keys [x y zoom label is-end]}]
  [:g {:transform (dm/fmt "translate(%, %) scale(%)" x y (/ 1 zoom))}
   [:foreignObject {:x (if is-end (- badge-width) 0)
                    :width badge-width
                    :height 24
                    :class (stl/css-case :badge-wrapper true
                                         :badge-wrapper-end is-end)}
    [:div {:class (stl/css :badge)} label]]])

(mf/defc shape-scope-area*
  [{:keys [shape zoom label]}]
  (let [{:keys [x y width height]} (dm/get-prop shape :selrect)]
    [:g {:transform (gsh/transform-str shape)}
     [:rect {:class (stl/css :outline)
             :x x
             :y y
             :width width
             :height height
             :stroke-width (/ 2 zoom)}]
     [:> scope-badge* {:x x
                       :y (+ y height (/ 6 zoom))
                       :zoom zoom
                       :label label}]]))

(mf/defc token-scope-areas*
  {::mf/wrap [mf/memo]}
  [{:keys [objects page zoom vbox]}]
  (let [tokens-lib (mf/deref refs/tokens-lib)
        shapes     (mf/with-memo [objects]
                     (into []
                           (filter (fn [shape]
                                     (and (some? (:token-scope shape))
                                          (not= (:id shape) uuid/zero)
                                          (not (:hidden shape)))))
                           (vals objects)))
        page-scope (:token-scope page)
        inset      (/ 2 zoom)]
    (when tokens-lib
      [:g {:class (stl/css :token-scope-areas)}
       (when (and page-scope vbox)
         [:*
          [:rect {:class (stl/css :outline)
                  :x (+ (:x vbox) inset)
                  :y (+ (:y vbox) inset)
                  :width (max 0 (- (:width vbox) (* 2 inset)))
                  :height (max 0 (- (:height vbox) (* 2 inset)))
                  :stroke-width (/ 4 zoom)}]
          [:> scope-badge* {:x (- (+ (:x vbox) (:width vbox)) (/ 16 zoom))
                            :y (- (+ (:y vbox) (:height vbox)) (/ 36 zoom))
                            :zoom zoom
                            :is-end true
                            :label (tr "workspace.token-scope-area.page"
                                       (scope-label tokens-lib page-scope))}]])
       (for [shape shapes]
         [:> shape-scope-area* {:key (dm/str "token-scope-" (:id shape))
                                :shape shape
                                :zoom zoom
                                :label (scope-label tokens-lib (:token-scope shape))}])])))
