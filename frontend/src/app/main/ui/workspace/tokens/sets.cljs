;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.sets
  (:require
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.common.types.tokens-status :as ctos]
   [app.config :as cf]
   [app.main.data.workspace.tokens.library-edit :as dwtl]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.context :as ctx]
   [app.main.ui.workspace.tokens.sets.helpers :as sets-helpers]
   [app.main.ui.workspace.tokens.sets.lists :refer [controlled-sets-list*]]
   [rumext.v2 :as mf]))

(defn- on-select-token-set-click [id]
  (st/emit! (dwtl/clear-tokens-paths)
            (dwtl/set-selected-token-set-id id)))

(defn- on-toggle-token-set-click [id]
  (st/emit! (dwtl/toggle-token-set id)))

(defn- on-toggle-token-set-group-click [path]
  (st/emit! (dwtl/toggle-token-set-group path)))

;; Stands in for the page and selection refs without the flag, so the list
;; does not re-render on every page change.
(def ^:private no-ref (atom nil))

(defn- context-set-ids
  "The ids of the sets active in the token scope of the selection: the
  selected shape's when one is selected, else the page's. Nil when no
  scope applies there, since the sets are then the file's."
  [tokens-status tokens-lib page selected]
  (let [shape-id (when (= 1 (count selected)) (first selected))
        chain    (cfo/scope-chain page (:objects page) shape-id)]
    (when (seq chain)
      (ctos/get-active-set-ids
       (cfo/effective-tokens-status tokens-status tokens-lib chain)))))

(mf/defc sets-list*
  [{:keys [selected new-path edition-id]}]

  (let [tokens-lib
        (mf/use-ctx ctx/tokens-lib)

        tokens-status
        (mf/use-ctx ctx/tokens-status)

        scoped?
        (contains? cf/flags :token-scoped-themes)

        page
        (mf/deref (if scoped? refs/workspace-page no-ref))

        selected-shapes
        (mf/deref (if scoped? refs/selected-shapes no-ref))

        context-set-ids
        (mf/with-memo [tokens-status tokens-lib page selected-shapes]
          (when (and tokens-status tokens-lib page)
            (context-set-ids tokens-status tokens-lib page selected-shapes)))

        token-set-in-context?
        (mf/with-memo [context-set-ids]
          (when context-set-ids
            (fn [set-id]
              (contains? context-set-ids set-id))))

        token-sets
        (some-> tokens-lib (ctob/get-set-tree))

        can-edit-tokens?
        (mf/use-ctx ctx/can-edit-tokens?)

        token-set-active?
        (mf/use-fn
         (mf/deps tokens-status)
         (fn [set-id]
           (ctos/set-active? tokens-status set-id)))

        token-set-group-active?
        (mf/use-fn
         (fn [group-path]
           ;; FIXME
           @(refs/token-sets-at-path-all-active group-path)))

        on-reset-edition
        (mf/use-fn
         (mf/deps can-edit-tokens?)
         (fn [_]
           (when can-edit-tokens?
             (st/emit! (dwtl/clear-token-set-edition)
                       (dwtl/clear-token-set-creation)))))

        on-start-edition
        (mf/use-fn
         (mf/deps can-edit-tokens?)
         (fn [id]
           (when can-edit-tokens?
             (st/emit! (dwtl/start-token-set-edition id)))))]

    [:> controlled-sets-list*
     {:tokens-lib tokens-lib
      :token-sets token-sets

      :is-token-set-active token-set-active?
      :is-token-set-group-active token-set-group-active?
      :is-token-set-in-context token-set-in-context?
      :on-select on-select-token-set-click

      :selected selected
      :new-path new-path
      :edition-id edition-id

      :origin "set-panel"
      :on-start-edition on-start-edition
      :on-reset-edition on-reset-edition

      :on-toggle-token-set on-toggle-token-set-click
      :on-toggle-token-set-group on-toggle-token-set-group-click
      :on-update-token-set (partial sets-helpers/on-update-token-set tokens-lib)
      :on-update-token-set-group sets-helpers/on-update-token-set-group
      :on-create-token-set sets-helpers/on-create-token-set}]))
