;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.tokens.scopes
  "Token scopes: the theme overrides pages and shapes carry in their
  `:token-scope` (see `cto/schema:token-scope`). The UI calls them token
  contexts."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.tokens.propagation :as dwtp]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

;; A scope without themes is a context that inherits everything, set up
;; to be filled in later.
(def empty-scope {:themes #{}})

(defn scope-holder?
  "Whether `shape` can have a token scope: a board, so also component
  mains and variants."
  [shape]
  (and (cfh/frame-shape? shape)
       (not= (:id shape) uuid/zero)))

(defn set-group-theme
  "The scope with the theme `theme-id` as the theme of `group`, or with
  no theme of that group when `theme-id` is nil."
  [scope tokens-lib group theme-id]
  (let [in-group? (fn [id] (= group (:group (ctob/get-theme tokens-lib id))))]
    (assoc scope :themes (cond-> (into #{} (remove in-group?) (:themes scope))
                           theme-id (conj theme-id)))))

(defn set-isolated
  "The scope in :isolated mode, or in the default :cascade mode."
  [scope isolated?]
  (if isolated?
    (assoc scope :mode :isolated)
    (dissoc scope :mode)))

(defn themes-label
  "The names of `themes` in theme group order, or a placeholder."
  [tokens-lib themes]
  (let [order (into {} (map-indexed (fn [i group] [group i])) (ctob/get-theme-groups tokens-lib))
        names (->> themes
                   (sort-by #(get order (:group %) -1))
                   (map :name)
                   (str/join " · "))]
    (if (str/blank? names)
      (tr "workspace.token-scope-area.no-themes")
      names)))

(defn scope-label
  "The names of the scope's themes, and a mark when it is isolated."
  [tokens-lib {:keys [themes mode]}]
  (let [names (themes-label tokens-lib (keep #(ctob/get-theme tokens-lib %) themes))]
    (if (= mode :isolated)
      (tr "workspace.token-scope-area.isolated" names)
      names)))

(defn selection-scope-chain
  "The scopes the selection resolves tokens with (see `cfo/scope-chain`):
  the selected shape's when one is selected, else the page's."
  [state]
  (let [page     (dsh/lookup-page state)
        selected (dsh/lookup-selected state)
        shape-id (when (= 1 (count selected)) (first selected))]
    (when page
      (cfo/scope-chain page (:objects page) shape-id))))

(defn set-page-token-scope
  "Sets the token scope of the current page, or removes it with nil, and
  propagates the tokens."
  [scope]
  (ptk/reify ::set-page-token-scope
    ptk/WatchEvent
    (watch [it state _]
      (let [page    (dsh/lookup-page state)
            changes (-> (pcb/empty-changes it)
                        (pcb/with-page page)
                        (pcb/mod-page {:token-scope scope}))]
        (rx/of (dch/commit-changes changes)
               (dwtp/propagate-workspace-tokens))))))

(defn set-shape-token-scope
  "Sets the token scope of a shape, or removes it with nil, and propagates
  the tokens."
  [shape-id scope]
  (ptk/reify ::set-shape-token-scope
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwsh/update-shapes [shape-id]
                                 (fn [shape]
                                   (if scope
                                     (assoc shape :token-scope scope)
                                     (dissoc shape :token-scope)))
                                 {:ignore-touched true})
             (dwtp/propagate-workspace-tokens)))))

(defn- holder-ids
  [state shape-ids]
  (let [objects (dsh/lookup-page-objects state)]
    (into [] (filter #(scope-holder? (get objects %))) shape-ids)))

(defn update-token-scopes
  "Sets the scope of each shape of `shape-ids` that can hold one to `f`
  of its scope (an empty one when it has none), and propagates."
  [shape-ids f]
  (ptk/reify ::update-token-scopes
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids (holder-ids state shape-ids)]
        (when (seq ids)
          (rx/of (dwsh/update-shapes ids
                                     #(assoc % :token-scope (f (or (:token-scope %) empty-scope)))
                                     {:ignore-touched true})
                 (dwtp/propagate-workspace-tokens)))))))

(defn assign-token-scope
  "Gives the selected shapes that can hold a scope a copy of `scope`, or
  removes theirs with nil (the global context), and propagates."
  [scope]
  (ptk/reify ::assign-token-scope
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids (holder-ids state (dsh/lookup-selected state))]
        (when (seq ids)
          (rx/of (dwsh/update-shapes ids
                                     #(if scope
                                        (assoc % :token-scope scope)
                                        (dissoc % :token-scope))
                                     {:ignore-touched true})
                 (dwtp/propagate-workspace-tokens)))))))

(defn remove-token-scopes
  "Removes the scopes of the shapes of `shape-ids` and propagates."
  [shape-ids]
  (ptk/reify ::remove-token-scopes
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwsh/update-shapes shape-ids #(dissoc % :token-scope)
                                 {:ignore-touched true})
             (dwtp/propagate-workspace-tokens)))))
