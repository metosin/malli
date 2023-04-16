(ns ^:no-doc malli.cherry
  (:refer-clojure :exclude [eval])
  (:require [cherry.embed :as cherry]
            [malli.core]))

(cherry/preserve-ns 'cljs.core)
(cherry/preserve-ns 'malli.core) ;; we could be more fine-grained here with another API fn, e.g (cherry/preserve-var 'malli.core/...)
(cherry/preserve-ns 'clojure.string)

(def cherry-opts {:aliases {'m 'malli.core
                            'str 'clojure.string}})

(defn eval
  ([code] (eval code nil))
  ([code _opts]
   (cond (or (symbol? code)
             (seq? code))
         (cherry/eval-form code cherry-opts)
         (string? code)
         (cherry/eval-string code cherry-opts)
         :else code)))

(set! malli.core/eval eval)
