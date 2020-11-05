(ns malli.schema.utils
  "Private utilities used in schema implementation."
  (:refer-clojure :exclude [record?])
  #?(:clj  (:require [clojure.string :as string])
     :cljs (:require
             goog.string.format
             [goog.object :as gobject]
             [goog.string :as gstring]
             [clojure.string :as string]))
  #?(:cljs (:require-macros [malli.schema.utils :refer [char-map]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous helpers

(defn type-of [x]
  #?(:clj (class x), :cljs (js* "typeof ~{}" x)))

(defn fn-schema-bearer
  "What class can we associate the fn schema with? In Clojure use the class of the fn; in
   cljs just use the fn itself."
  [f] #?(:clj (class f), :cljs f))

(defn format* [fmt & args]
  (apply #?(:clj format, :cljs gstring/format) fmt args))

(def max-value-length (atom 19))

(defmacro char-map []
  clojure.lang.Compiler/CHAR_MAP)

(defn unmunge
  "TODO: eventually use built in demunge in latest cljs."
  [s]
  (->> (char-map)
       (sort-by #(- (count (second %))))
       (reduce (fn [^String s [to from]] (string/replace s from (str to))) s)))

(defn fn-name
  "A meaningful name for a function that looks like its symbol, if applicable."
  [f]
  #?(:cljs (let [[_ s] (re-matches #"#object\[(.*)\]" (pr-str f))]
             (if (= "Function" s)
               "function"
               (->> s demunge (re-find #"[^/]+(?:$|(?=/+$))"))))
     :clj  (let [s (.getName (class f))
                 slash (.lastIndexOf s "$")
                 raw (unmunge
                       (if (>= slash 0)
                         (str (subs s 0 slash) "/" (subs s (inc slash)))
                         s))]
             (string/replace raw #"^clojure.core/" ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Registry for attaching schemas to classes, used for defn and defrecord

#?(:clj  (let [^java.util.Map +class-schemata+ (java.util.Collections/synchronizedMap (java.util.WeakHashMap.))]
           (defn declare-class-schema! [klass schema]
             "Globally set the schema for a class (above and beyond a simple instance? check).
            Use with care, i.e., only on classes that you control.  Also note that this
            schema only applies to instances of the concrete type passed, i.e.,
            (= (class x) klass), not (instance? klass x)."
             (assert (class? klass)
                     (format* "Cannot declare class schema for non-class %s" (class klass)))
             (.put +class-schemata+ klass schema))

           (defn class-schema [klass]
             "The last schema for a class set by declare-class-schema!, or nil."
             (.get +class-schemata+ klass)))

   :cljs (do (defn declare-class-schema! [klass schema]
               (gobject/set klass "schema$utils$schema" schema))

             (defn class-schema [klass]
               (gobject/get klass "schema$utils$schema"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities for fast-as-possible reference to use to turn fn schema validation on/off

(def use-fn-validation
  "Turn on run-time function validation for functions compiled when
   s/compile-fn-validation was true -- has no effect for functions compiled
   when it is false."
  ;; specialize in Clojure for performance
  #?(:clj (java.util.concurrent.atomic.AtomicReference. false)
     :cljs (atom false)))
