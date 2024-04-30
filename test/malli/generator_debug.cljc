(ns malli.generator-debug
  "Drop-in replacement for clojure.test.check.generators that returns AST's
  instead of generators."
  (:refer-clojure :exclude [vector char keyword boolean not-empty symbol])
  (:require [clojure.core :as cc]))

(defmacro such-that [& args] (let [args (vec args)] `{:op :such-that :args-form '~args :args ~args}))
(def any {:op :any})
(def any-printable {:op :any-printable})
(defn double* [& args] {:op :double* :args args})
(defmacro fmap [& args] (let [args (vec args)] `{:op :fmap :args-form '~args :args ~args}))
(defmacro vector
  ([generator] {:op :vector :generator generator})
  ([generator num-elements] {:op :vector :generator generator :num-elements num-elements})
  ([generator min-elements max-elements]
   {:op :vector :generator generator :min-elements min-elements :max-elements max-elements}))
(defmacro vector-distinct [& args] (let [args (vec args)] `{:op :vector-distinct :args-form '~args :args ~args}))
(defmacro vector-distinct-by [& args] (let [args (vec args)] `{:op :vector-distinct-by :args-form '~args :args ~args}))
(def char {:op :char})
(def nat {:op :nat})
(def char-alphanumeric {:op :char-alphanumeric})
(def string-alphanumeric {:op :string-alphanumeric})
(defn sized [& args] {:op :sized :args args})
(defn return [value] {:op :return :value value})
(defn one-of [generators] {:op :one-of :generators generators})
(defn tuple [& generators] {:op :tuple :generators (vec generators)})
(def ^:private ^:dynamic *recursion-depth* 0)
(defn recursive-gen [rec scalar]
  (let [target (cc/keyword (str "recur" *recursion-depth*))]
    {:op :recursive-gen
     :target target
     :rec-gen (binding [*recursion-depth* (inc *recursion-depth*)]
                (rec {:op :recur
                      :target target}))
     :scalar-gen scalar}))
(def keyword {:op :keyword})
(def keyword-ns {:op :keyword-ns})
(def symbol {:op :symbol})
(def symbol-ns {:op :symbol-ns})
(def s-pos-int {:op :s-pos-int})
(def s-neg-int {:op :s-neg-int})
(defn elements [coll] {:op :elements :coll coll})
(defn large-integer* [& args] {:op :large-integer* :args args})
(def boolean {:op :boolean})
(def uuid {:op :uuid})
(defn not-empty [gen] {:op :not-empty :gen gen})
(defn generator? [& args] (assert nil "no stub for generator?"))
(defn call-gen [& args] (assert nil "no stub for call-gen"))
(defn make-size-range-seq [& args] (assert nil "no stub for make-size-range-seq"))
(defn lazy-random-states [& args] (assert nil "no stub for lazy-random-states"))
