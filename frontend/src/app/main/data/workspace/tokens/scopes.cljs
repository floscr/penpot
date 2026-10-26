;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.tokens.scopes
  "Token scopes: the theme overrides pages and shapes carry in their
  `:token-scope` (see `cto/schema:token-scope`)."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.tokens.propagation :as dwtp]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

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
