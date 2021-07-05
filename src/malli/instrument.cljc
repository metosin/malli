(ns malli.instrument
  (:require [malli.core :as m]))

(def ^:private instrumented* (atom nil))

(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-all [] (constantly true))

(defn instrument!
  ([] (instrument! nil))
  ([{:keys [mode filters] :or {mode :instrument, filters [(-filter-all)]}}]
   (doseq [[n d] (m/function-schemas), [s d] d]
     (if (some #(% n s d) filters)
       (if-let [v (find-var (symbol (str n "/" s)))]
         (case mode
           :instrument (let [original-fn (or (::original-fn (meta v)) (deref v))]
                         (swap! instrumented* (fnil assoc {}) v d)
                         (alter-meta! v assoc ::original-fn original-fn)
                         (alter-var-root v (constantly (m/-instrument d original-fn)))
                         (println "instrumented" v (:schema d)))
           :unstrument (when-let [original-fn (::original-fn (meta v))]
                         (swap! instrumented* (fn [s] (some-> s (dissoc v) (seq) (->> (into {})))))
                         (alter-meta! v dissoc ::original-fn)
                         (alter-var-root v (constantly original-fn))
                         (println "unstrumented" v (:schema d)))))))))

(defn instrumented [] @instrumented*)

(defn start! []
  (instrument!)
  )

;; demo

(defn kikka [x] (* x x))
(m/=> kikka [:=> [:cat :int] [:int {:max 6}]])

(m/=> kakka [:=> [:cat :int] [:int {:max 6}]])

(instrument!)
(instrument! {:filters [(-filter-all)]})
(instrument! {:filters [(-filter-ns 'malli.instrument)]})
(instrument! {:mode :unstrument})

(instrumented)

(kikka 12)

(comment

  (defn kukka
    "kukka"
    {:malli/schema [:=> [:cat :int] [:int {:max 6}]]
     :malli/scope #{:input :output}}
    [x] (* x x))


  (def s (atom nil))
  (add-watch s ::instrument (fn [key atom old-state new-state]
                              (prn key atom old-state new-state)))
  (swap! s conj 1))
