(ns malli.adapter.spec1
  "Cannot validate values containing ::m/invalid or ::s/invalid."
  (:require [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.util :as mu]
            [clojure.spec.alpha :as s]))

;;simpler cljs compat
(defonce -malli-gen* (atom nil))

(defmacro malli
  ([m] `(-malli {:m ~m}))
  ([m options] `(-malli {:m ~m :options ~options})))

(defn -malli
  [{:keys [m options]}]
  (assert (or (m/schema? m)
              (some? options))
          "malli: Must provide options or Schema")
  (let [m (m/schema m options)]
    (reify
      m/Schema
      (-validator [_] (m/-validator m))
      (-explainer [_ path] (m/-explainer m path))
      (-parser [_] (m/-parser m))
      (-unparser [_] (m/-unparser m))
      (-transformer [_ transformer method options] (m/-transformer m transformer method options))
      (-walk [this walker path options] (m/-walk this walker path options))
      (-properties [_] (m/-properties m))
      (-options [_] (m/-options m))
      (-children [_] (m/-children m))
      (-parent [_] (m/-parent m))
      (-form [_] (m/-form m))
      s/Spec
      (conform* [_ x] (let [p (m/parse m x)]
                        (if (miu/-invalid? p)
                          ::s/invalid
                          ;;TODO
                          p)))
      (unform* [_ x] (let [u (m/unparse m x)]
                       (if (miu/-invalid? u)
                         ::s/invalid
                         u)))
      (explain* [_ path via in x]
        (->> ((m/-explainer m path) x in [])
             (mapv (fn [{:keys [value in path schema]}]
                     {:path path
                      :pred (-> schema (malli options) s/form)
                      :via via
                      :in in #_(into in path) ;;FIXME
                      :val value}))))
      (gen* [_ overrides path rmap]
        (if-some [gen* @-malli-gen*]
          (gen* m overrides path rmap)
          (throw (ex-info "must require malli.adapter.spec1.generator for spec1 adapter generator"
                          {}))))
      (with-gen* [_ gfn]
        (malli
          (m/-update-properties m assoc :gen #(gfn))))
      (describe* [_]
        (list `malli (m/form m))))))

(defn- -spec-into-schema []
  (mu/-util-schema {:type ::spec
                    :childs 1 :min 1 :max 1
                    :fn (fn [_ [s :as children] options]
                          [children children (-spec {:s s :options options})])}))

(defn -spec [{:keys [s options parent]}]
  (assert (or (qualified-keyword? s)
              (s/spec? s)))
  (reify
    s/Specize
    (specize* [s] s)
    (specize* [s _] s)
    s/Spec
    (conform* [_ x] (s/conform s x))
    (unform* [_ x] (s/unform s x))
    (explain* [_ path via in x] (s/explain* (s/spec s) path via in x))
    (gen* [_ overrides path rmap] (s/gen* (s/spec s) overrides path rmap))
    (with-gen* [_ gfn] (s/with-gen s gfn))
    (describe* [_] s)
    m/Schema
    (-validator [_] (fn _validator [x] (s/valid? s x)))
    (-explainer [_ path] (fn _explainer [x in acc]
                           (let [r (s/explain-data* (s/spec s) path [] in x)]
                             (if (nil? r)
                               acc
                               (let [{::s/keys [problems spec value]} r]
                                 (reduce
                                   (fn [acc {path' :path in' :in :keys [pred val via]}]
                                     (conj acc {:path path'
                                                :in in'
                                                :schema (-spec {:s (eval `(s/spec ~pred)) :options options})
                                                :value val}))
                                   acc problems))))))
    (-parser [_] #(let [c (s/conform* (s/spec s) %)]
                       (if (s/invalid? c)
                         ::m/invalid
                         c)))
    (-unparser [_] #(let [u (s/unform* (s/spec s) %)]
                         (if (s/invalid? u)
                           ::m/invalid
                           u)))
    (-transformer [this transformer method options]
      (assert nil "TODO spec -> -transformer"))
    (-walk [this walker path options] (m/-walk-leaf this walker path options))
    (-properties [_] {})
    (-options [_] options)
    (-children [_] [s])
    (-parent [_] (-spec-into-schema))
    (-form [_] [::spec (cond-> s
                         (s/spec? s) s/form)])))

(defmacro spec
  ([s] `(-spec {:s ~s}))
  ([s options] `(-spec {:s ~s :options ~options})))

(defn schemas []
  {::spec (-spec-into-schema)})
