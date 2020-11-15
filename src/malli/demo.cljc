(ns malli.demo)

(require '[malli.schema :as ms])

(ms/defn fun :- [:tuple int? pos-int?]
  "returns a tuple of a number & squared"
  ([x :- int?]
   (fun x x))
  ([x :- int?, y :- int?]
   [x (* x x)]))

(ms/defn square :- int?
  [x :- int?]
  (* x x))

;; clj-kondo doing static analysis
(comment
  (fun 1 "2")
  (square "1"))


;; emit clj-kondo type-linting info
(require '[malli.clj-kondo :as mc])

(-> *ns*
    (mc/from-ns)
    (mc/linter-config)
    (mc/save!))
