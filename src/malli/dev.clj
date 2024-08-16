(ns malli.dev
  (:require [malli.clj-kondo :as clj-kondo]
            [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [malli.instrument :as mi]))

(defn -log!
  ([text] (-log! text (pretty/-printer)))
  ([text printer] (pretty/-log! text printer)))

(def ^:private ^:dynamic *forms* nil)

(defn -capture-fail!
  ([] (-capture-fail! nil))
  ([{:keys [report] :or {report (pretty/reporter)}}]
   (alter-var-root
    #'m/-fail!
    (fn [f]
      (-> (fn -fail!
            ([type] (-fail! type nil))
            ([type data] (let [e (m/-exception type data)]
                           (report type data)
                           (throw e)))
            ([type data options]
             (let [data (assoc data ::forms (some-> *forms* deref))
                   e (m/-exception type data)]
               (report type data)
               (throw e))))
          (with-meta {::original (::original (meta f) f)}))))))

(defn -uncapture-fail! []
  (alter-var-root #'m/-fail! (fn [f] (::original (meta f) f))))

(def ^:dynamic *level* nil)

(defn- ensure-forms [f]
  (binding [*forms* (or *forms* (atom []))
            *level* (or (some-> *level* inc) 0)]
    (f *forms* *level*)))

(defn -capture-schema-ctors! []
  (alter-var-root #'m/schema
                  (fn [f]
                    (let [f (::original (meta f) f)]
                      (fn this
                        ([?schema] (this ?schema nil))
                        ([?schema options]
                         (ensure-forms
                           (fn [forms level]
                             (swap! forms conj {:level level :form ?schema})
                             (f ?schema options))))))))
  (alter-var-root #'m/into-schema
                  (fn [f]
                    (let [f (::original (meta f) f)]
                      (fn this
                        ([type properties children] (this type properties children nil))
                        ([type properties children options]
                         (ensure-forms
                           (fn [forms level]
                             (swap! forms conj {:level level :form [type properties children]})
                             (f type properties children options))))))))
  (alter-var-root #'m/-lookup!
                  (fn [-lookup!]
                    (let [-lookup! (::original (meta -lookup!) -lookup!)]
                      (fn [?schema ?form f rec options]
                        (ensure-forms
                          (fn [forms level]
                            (swap! forms conj {:level level :form ?schema})
                            (-lookup! ?schema ?form f rec options)))))))
  (alter-var-root #'m/-lookup
                  (fn [f]
                    (let [f (::original (meta f) f)]
                      (fn [?schema options]
                        (ensure-forms
                          (fn [forms level]
                            (swap! forms conj {:level level :form ?schema})
                            (f ?schema options))))))))

(defn -uncapture-schema-ctors! []
  (alter-var-root #'m/-lookup (fn [f] (::original (meta f) f)))
  (alter-var-root #'m/-lookup! (fn [f] (::original (meta f) f)))
  (alter-var-root #'m/into-schema (fn [f] (::original (meta f) f)))
  (alter-var-root #'m/schema (fn [f] (::original (meta f) f))))

(comment
  (-capture-fail!)
  (-capture-schema-ctors!)
  (m/schema [:map [:a [:tuple nil nil]]])
;; -- Schema Creation Error ---------------------------------------- Thread:1583 --
;; 
;; Invalid Schema
;; 
;;   nil
;; 
;; Surrounding Syntax
;; 
;; [:map [:a [:tuple nil nil]]]
;;  :map
;;   :map
;;  [#IntoSchema{:type :map} nil [[:a [:tuple nil nil]]]]
;;   #IntoSchema{:type :map}
;;   [:tuple nil nil]
;;    :tuple
;;     :tuple
;;    [#IntoSchema{:type :tuple} nil [nil]]
;;     #IntoSchema{:type :tuple}
;;     nil
;;      nil
;;       nil
;; 
;; More information
;; 
;;   https://cljdoc.org/d/metosin/malli/CURRENT
;; 
;; --------------------------------------------------------------------------------

  )

;;
;; Public API
;;

(defn stop!
  "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
  []
  (remove-watch @#'m/-function-schemas* ::watch)
  (->> (mi/unstrument!) (count) (format "unstrumented %d function vars") (-log!))
  (clj-kondo/save! {})
  (-uncapture-schema-ctors!)
  (-uncapture-fail!)
  (-log! "dev-mode stopped"))

(defn start!
  "Collects defn schemas from all loaded namespaces and starts instrumentation for
   a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]]
   for possible options. Re-instruments if the function schemas change. Also emits
   clj-kondo type annotations."
  ([] (start! {:report (pretty/reporter)}))
  ([options]
   (with-out-str (stop!))
   (-capture-fail! options)
   (-capture-schema-ctors!)
   (mi/collect! {:ns (all-ns)})
   (let [watch (bound-fn [_ _ old new]
                 (->> (for [[n d] (:clj new)
                            :let [no (get-in old [:clj n])]
                            [s d] d
                            :when (not= d (get no s))]
                        [[n s] d])
                      (into {})
                      (reduce-kv assoc-in {})
                      (assoc options :data)
                      (mi/instrument!))
                 (clj-kondo/emit! options))]
     (add-watch @#'m/-function-schemas* ::watch watch))
   (let [count (->> (mi/instrument! options) (count))]
     (when (pos? count) (-log! (format "instrumented %d function vars" count))))
   (clj-kondo/emit! options)
   (-log! "dev-mode started")))
