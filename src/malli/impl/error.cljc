(ns malli.impl.error)

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
