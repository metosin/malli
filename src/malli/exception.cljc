(ns malli.exception)

#_(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type) {:type type, :data data}))))

(defn get-message [e]
  #?(:clj (.getMessage ^Exception e) :cljs (ex-message e)))

(defmulti format-exception (fn [type _ _] type) :default ::default)

(defn exception [e]
  (let [data (ex-data e)
        message (format-exception (:type data) (get-message e) (:data data))]
    (ex-info message (assoc (or data {}) ::cause e))))

;;
;; Formatters
;;

(defmethod format-exception ::default [_ message data]
  (str message (if data (str "\n\n" (pr-str data)))))

(defmethod format-exception :malli.core/invalid-schema [_ _ {:keys [key schema] :as data}]
  (str "Invalid Schema"
       (some->> (or key schema) (str ": "))
       (some->> (dissoc data :key) pr-str (str "\n\n"))))


