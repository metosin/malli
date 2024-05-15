(ns malli.adapter.spec1
  "Cannot validate values containing ::m/invalid or ::s/invalid."
  #?(:cljs (:require-macros malli.adapter.spec1))
  (:require [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.util :as mu]
            [clojure.spec.alpha :as s]))

;;simpler cljs compat
(defonce ^:internal -malli-gen* (atom nil))

#?(:clj
   (defmacro malli
     ([m] `(-malli {:m ~m}))
     ([m options] `(-malli {:m ~m :options ~options}))))

(defn ^:internal -malli
  [{:keys [m options]}]
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

(defn ^:internal -spec-into-schema [{:keys [type] :or {type ::spec}}]
  (reify
    m/IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      ;; [::spec ::kw]
      ;; [::spec form spec]
      (m/-check-children! type properties children 1 2)
      ^{:type ::m/schema}
      (let [s (peek children)
            form (delay (m/-create-form type properties children options))
            cache (m/-create-cache options)]
        (reify
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
                                                      :schema (m/schema [:not {::pred pred} :any])
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
            (m/-intercepting (m/-value-transformer transformer this method options)))
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)

          s/Specize
          (specize* [s] s)
          (specize* [s _] s)
          s/Spec
          (conform* [_ x] (s/conform s x))
          (unform* [_ x] (s/unform s x))
          (explain* [_ path via in x] (s/explain* (s/spec s) path via in x))
          (gen* [_ overrides path rmap] (s/gen* (s/spec s) overrides path rmap))
          (with-gen* [_ gfn] (s/with-gen s gfn))
          (describe* [_] (if (keyword? s) s (s/describe s))))))))

(defn schemas []
  {::spec (-spec-into-schema {})})

#?(:clj
   (defmacro spec
     ([s] `(spec ~s nil))
     ([s options] `(let [s# ~s]
                     (m/schema (if (keyword? s#)
                                 [::spec s#]
                                 [::spec '~s s#])
                               (update ~options :registry #(mr/composite-registry (schemas) (or % {}))))))))
