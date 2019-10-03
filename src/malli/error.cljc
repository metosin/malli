(ns malli.error
  (:require [malli.core :as m]))

;; TODO: complete this
(def default-errors
  {::unknown {:error/message {:en "unknown error"}}
   ::m/missing-key {:error/message {:en "missing required key"}}
   'int? {:error/message {:en "should be an int"}}})

(defrecord SchemaError [value message])

(defn -maybe-localized [x locale]
  (if (map? x) (get x locale (get x :en)) x))

(defn -message [{:keys [value schema]} x locale opts]
  (or (if-let [fn (-maybe-localized (:error/fn x) locale)] ((m/eval fn) schema value opts))
      (-maybe-localized (:error/message x) locale)))

;;
;; public api
;;

(defn error-message
  ([error]
   (error-message error nil))
  ([{:keys [schema] :as error} {:keys [errors locale] :or {errors default-errors} :as opts}]
   (or (-message error (m/properties schema) locale opts)
       (-message error (errors (m/name schema)) locale opts)
       (-maybe-localized (-> errors ::unknown :error/message) locale))))

(defn merge-errors
  ([explanation]
   (merge-errors explanation nil))
  ([explanation {:keys [errors locale] :or {errors default-errors} :as opts}]
   (reduce
     (fn [acc error]
       (if (= ::m/missing-key (:type error))
         (update-in acc (conj (:in error) (::m/key error)) ->SchemaError
                    (-maybe-localized (:error/message (::m/missing-key errors)) locale))
         (update-in acc (:in error) (fn [x] (->SchemaError x (error-message error opts))))))
     (:value explanation)
     (:errors explanation))))
