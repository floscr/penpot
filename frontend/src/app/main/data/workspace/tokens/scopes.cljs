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
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

;; A scope without themes: a context that inherits everything
(def ^:private empty-scope {:themes #{}})

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

(defn theme-path
  "A theme as \"Group / Theme\", or its name when it has no group."
  [{:keys [group name]}]
  (if (seq group)
    (str group " / " name)
    name))

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

(defn- with-propagation
  "`event`, which changes scopes, and the propagation of the tokens that
  resolve differently because of it, as a single undo entry: undoing it
  brings the scopes back too, not only the values."
  [event]
  (let [undo-id (js/Symbol)]
    (rx/of (dwu/start-undo-transaction undo-id)
           event
           (dwtp/propagate-workspace-tokens)
           (dwu/commit-undo-transaction undo-id))))

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
        (with-propagation (dch/commit-changes changes))))))

(defn- set-scopes
  "Sets the scopes of the shapes of `ids` to `f` of each shape (removes
  them where it gives nil) and propagates."
  [ids f]
  (with-propagation
    (dwsh/update-shapes ids
                        (fn [shape]
                          (if-let [scope (f shape)]
                            (assoc shape :token-scope scope)
                            (dissoc shape :token-scope)))
                        {:ignore-touched true})))

(defn set-shape-token-scope
  "Sets the token scope of a shape, or removes it with nil, and propagates
  the tokens."
  [shape-id scope]
  (ptk/reify ::set-shape-token-scope
    ptk/WatchEvent
    (watch [_ _ _]
      (set-scopes [shape-id] (constantly scope)))))

(defn- holder-ids
  [state shape-ids]
  (let [objects (dsh/lookup-page-objects state)]
    (into [] (filter #(scope-holder? (get objects %))) shape-ids)))

(defn- new-scope
  "The scope a shape (the page with nil) starts with. Under another scope
  it is empty and inherits everything; at the top it is a copy of the
  file's themes, since a top scope does not follow them."
  [state shape-id]
  (let [page      (dsh/lookup-page state)
        objects   (:objects page)
        parent-id (when shape-id (get-in objects [shape-id :parent-id]))]
    (if (and parent-id (seq (cfo/scope-chain page objects parent-id)))
      empty-scope
      (cfo/file-scope (dsh/lookup-tokens-status state) (dsh/lookup-tokens-lib state)))))

(defn update-token-scopes
  "Sets the scope of each shape of `shape-ids` that can hold one to `f`
  of its scope (see `new-scope` when it has none), and propagates."
  [shape-ids f]
  (ptk/reify ::update-token-scopes
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids   (holder-ids state shape-ids)
            bases (into {} (map (fn [id] [id (new-scope state id)])) ids)]
        (when (seq ids)
          (set-scopes ids #(f (or (:token-scope %) (get bases (:id %))))))))))

(defn- resolved-scope
  "A scope with the themes the context `source` resolves with: :page or a
  shape id."
  [state source]
  (let [page   (dsh/lookup-page state)
        lib    (dsh/lookup-tokens-lib state)
        chain  (cfo/scope-chain page (:objects page) (when (uuid? source) source))
        status (cfo/effective-tokens-status (dsh/lookup-tokens-status state) lib chain)]
    (cfo/file-scope status lib)))

(defn assign-token-scope
  "Gives the selected shapes that can hold a scope the themes the context
  `source` (:page or a shape id) resolves with, so they look the same.
  Propagates."
  [source]
  (ptk/reify ::assign-token-scope
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids   (holder-ids state (dsh/lookup-selected state))
            scope (resolved-scope state source)]
        (when (seq ids)
          (set-scopes ids (constantly scope)))))))

(defn add-token-scope
  "Gives the selected boards that have no scope an empty one, or the page
  when nothing that can hold a scope is selected."
  []
  (ptk/reify ::add-token-scope
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids  (holder-ids state (dsh/lookup-selected state))
            page (dsh/lookup-page state)]
        (cond
          (seq ids)
          (rx/of (update-token-scopes ids identity))

          (nil? (:token-scope page))
          (rx/of (set-page-token-scope (new-scope state nil))))))))

(defn remove-token-scopes
  "Removes the scopes of the shapes of `shape-ids` and propagates."
  [shape-ids]
  (ptk/reify ::remove-token-scopes
    ptk/WatchEvent
    (watch [_ _ _]
      (set-scopes shape-ids (constantly nil)))))
