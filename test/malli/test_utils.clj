(ns malli.test-utils
  (:require [malli.core :as m]))

(defn with-schema-forms [result]
  (some-> result
          (update :schema m/form)
          (update :errors (partial map (fn [error]
                                         (-> error
                                             (update :schema m/form)
                                             (update :type (fnil identity nil))
                                             (update :message (fnil identity nil))
                                             (dissoc :check)))))))
