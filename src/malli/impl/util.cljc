(ns malli.impl.util
  #?(:clj (:import [clojure.lang MapEntry])))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))

(defn stateful-mapv [f coll s]
  (let [s (volatile! s)]
    [(mapv (fn [v] (let [[v* s*] (f v @s)] (vreset! s s*) v*))
           coll)
     @s]))

(defn -fail!
  ([type]
   (-fail! type nil))
  ([type data]
   (-fail! type nil data))
  ([type message data]
   (throw (ex-info (str type " " (pr-str data) message) {:type type, :data data}))))

(defrecord SchemaError [path in schema value type message])

(defn -error
  ([path in schema value]
   (->SchemaError path in schema value nil nil))
  ([path in schema value type]
   (->SchemaError path in schema value type nil)))
