(ns malli.poly
  (:refer-clojure :exclude [eval type -deref deref -lookup -key assert])
  #?(:cljs (:require-macros malli.core))
  (:require [clojure.walk :as walk]
            [clojure.core :as c]
            [malli.core :as m]
            [malli.impl.regex :as re]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.sci :as ms]
            [malli.poly-protocols :refer [AllSchema -bounds -inst]]))

(declare inst)

(defn- -all-binder-bounds [binder]
  (m/-vmap (fn [b]
             (if (simple-ident? b)
               {:kind :Schema
                :default :any
                :lower nil
                :upper :any}
               (if (and (vector? b)
                        (= 2 (count b))
                        (simple-ident? (first b)))
                 {:kind :Schema
                  :default (second b)
                  :lower nil
                  :upper (second b)}
                 (if (and (map? b)
                          (simple-ident? (:name b)))
                   (dissoc b :name)
                   (m/-fail! ::invalid-all-binder {:binder binder})))))
           binder))

(defn- -visit-binder-names [binder f]
  (m/-vmap (fn [b]
             (if (simple-ident? b)
               (f b)
               (if (and (vector? b)
                        (= 2 (count b))
                        (simple-ident? (first b)))
                 (update b 0 f)
                 (if (and (map? b)
                          (simple-ident? (:name b)))
                   (update b :name f)
                   (m/-fail! ::invalid-all-binder {:binder binder})))))
           binder))

(defn -all-binder-names [binder]
  (let [vol (volatile! [])]
    (-visit-binder-names binder #(do (vswap! vol conj %) %))
    @vol))

(defn- -find-allowed-kw [base vforbidden]
  (if-not (@vforbidden base)
    base
    (loop [i 0]
      (let [base (keyword (str (name base) i))]
        (if-not (@vforbidden base)
          (do (vswap! vforbidden conj base)
              base)
          (recur (inc i)))))))

(defn- -alpha-rename [s vforbidden options]
  (let [inner (fn [this s path options]
                (case (m/type s)
                  :all (-alpha-rename s vforbidden options)
                  (m/-walk s this path options)))
        outer (fn [s path children options]
                (case (m/type s)
                  ::m/val (first children)
                  (m/-set-children s children)))
        walk (fn [s]
               (inner
                 (reify m/Walker
                   (-accept [_ s path options] true)
                   (-inner [this s path options] (inner this s path options))
                   (-outer [_ schema path children options]
                     (outer schema path children options)))
                 s
                 []
                 (assoc options
                        ::m/walk-refs false
                        ::m/walk-schema-refs false
                        ::m/walk-entry-vals true)))
        [binder body] (m/children s)
        names (-all-binder-names binder)
        bounds (-all-binder-bounds binder)
        renames (into {} (map (fn [n]
                                [n (-find-allowed-kw n vforbidden)]))
                      names)
        binder (-visit-binder-names binder renames)
        defaults (into {} (map-indexed
                            (fn [i n]
                              (let [{:keys [default]} (nth bounds i)
                                    rename (renames n)]
                                [n (m/form
                                     (m/-update-properties
                                       (m/schema default options)
                                       #(assoc % ::alpha-rename rename))
                                     options)])))
                       names)
        invert (into {} (map (fn [[k v]]
                               [v (renames k)]))
                     defaults)
        body (-> body
                 (->> (walk/postwalk-replace defaults))
                 (m/schema options)
                 walk
                 (m/form options)
                 (->> (walk/postwalk-replace invert)))]
    (m/schema [:all binder body] options)))

(defn- -inst* [binder body insts options]
  (when-not (= (count insts)
               (count binder))
    (m/-fail! ::wrong-number-of-schemas-to-inst
              {:binder binder :schemas insts}))
  (let [kws (-all-binder-names binder)
        bounds (-all-binder-bounds binder)
        insts (mapv (fn [bound s]
                      ;;TODO regex kinds like [:* :Schema] that allow splicing schemas like
                      ;; [:all [[Xs [:* :Schema]]] [:=> [:cat Xs] :any]]
                      ;; which can instantiate to [:=> [:cat [:* :int] :any]] rather than just
                      ;; [:=> [:cat [:schema [:* :int]] :any]]
                      (case (:kind bound)
                        :Schema (let [{:keys [upper lower]} bound
                                      s (m/form s options)
                                      upper (m/form upper options)]
                                  (when (some? lower)
                                    (m/-fail! ::nyi-lower-bounds))
                                  (m/form
                                    [:schema ;;disallow regex splicing
                                     (if (or (= s upper)
                                             (= :any upper))
                                       s
                                       [:and s upper])]
                                    options))))
                    bounds insts)
        vforbidden-kws (volatile! (set kws))
        _ (walk/postwalk (fn [v]
                           (when (keyword? v)
                             (vswap! vforbidden-kws conj v))
                           v)
                         insts)
        [binder body] (-> [:all binder body]
                          (m/schema options)
                          (-alpha-rename vforbidden-kws options)
                          m/children)
        kws (-all-binder-names binder)]
    (-> (walk/postwalk-replace (zipmap kws insts) body)
        (m/schema options))))

(defn -all-binder-defaults [binder]
  (mapv :default (-all-binder-bounds binder)))

(defn -all-schema [_]
  ^{:type ::m/into-schema}
  (reify m/IntoSchema
    (-type [_] :all)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children {::m/keys [function-checker] :as options}]
      (m/-check-children! :all properties children 2 2)
      (let [[binder body] children
            form (delay (m/-simple-form parent properties children identity options))
            cache (m/-create-cache options)
            self-inst (delay (inst [:all binder body] options))
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        ^{:type ::m/schema}
        (reify
          m/Schema
          (-validator [this]
            (if-let [checker (->checker this)]
              (let [validator (fn [x] (nil? (checker x)))]
                (fn [x] (and (ifn? x) (validator x))))
              ifn?))
          (-explainer [this path]
            (if-let [checker (->checker this)]
              (fn explain [x in acc]
                (if (not (ifn? x))
                  (conj acc (miu/-error path in this x))
                  (if-let [res (checker x)]
                    (conj acc (assoc (miu/-error path in this x) :check res))
                    acc)))
              (let [validator (m/-validator this)]
                (fn explain [x in acc]
                  (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))))
          (-parser [this]
            (let [validator (m/-validator this)]
              (fn [x] (if (validator x) x ::m/invalid))))
          (-unparser [this] (m/-parser this))
          (-transformer [_ _ _ _])
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          AllSchema
          (-bounds [_] (-all-binder-bounds binder))
          (-inst [_ insts] (-inst* binder body (or insts (-all-binder-defaults binder)) options))
          m/FunctionSchema
          (-function-schema? [this] (m/-function-schema? @self-inst))
          (-function-schema-arities [this] (m/-function-schema-arities @self-inst))
          (-function-info [this] (m/-function-info @self-inst))
          (-instrument-f [schema props f options] (m/-instrument-f @self-inst props f options))
          m/Cached
          (-cache [_] cache)
          m/LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (m/-set-assoc-children this key value))
          m/RefSchema
          (-ref [_])
          (-deref [_] @self-inst))))))

(defn- -find-kws [vol form]
  (walk/postwalk (fn [v]
                   (when (simple-keyword? v)
                     (vswap! vol conj v))
                   v)
                 form))

(defn- -rename-all-binder [forbidden-kws binder]
  (-visit-binder-names
    binder
    (fn [k]
      (if (@forbidden-kws k)
        (loop [i 0]
          (let [k' (keyword (str (name k) i))]
            (if (@forbidden-kws k')
              (recur (inc i))
              (do (vswap! forbidden-kws conj k')
                  k'))))
        k))))

(defn -all-form [binder body]
  (let [nbound (count binder)
        binder (-visit-binder-names binder (fn [k]
                                             (when-not (simple-symbol? k)
                                               (m/-fail! ::binder-must-use-simple-symbols {:k k}))
                                             (keyword k)))
        forbidden-kws (doto (volatile! #{})
                        (-find-kws (apply body (repeatedly nbound random-uuid))))
        binder (-rename-all-binder forbidden-kws binder)
        body (apply body (-all-binder-names binder))]
    [:all binder body]))

#?(:clj
   (defmacro all
     "Children of :all are read-only. Only construct an :all with this macro.
     Children will not be walked.

     The only public interface for :all is inst and -bounds.

     Treat type variables as opaque variables in body. i.e., only pass them around,
     don't inspect or test them.

     Use deref to instantiate the body with each type variable's upper bounds."
     [binder body]
     (let [bv (mapv (fn [b]
                      (if (symbol? b)
                        b
                        (if (vector? b)
                          (first b)
                          (if (map? b)
                            (:name b)
                            (m/-fail! ::bad-all-binder {:binder binder})))))
                    binder)]
       `(-all-form '~binder (fn ~bv ~body)))))

(defn inst
  "Instantiate an :all schema with a vector of schemas. If a schema
  is nil, its upper bound will be used. If ?schemas is nil or not provided, same as
  vector of nils. ?schemas-or-options are treated as options if map?, otherwise ?schemas."
  ([?all] (inst ?all nil nil))
  ([?all ?schemas-or-options] (let [options? (map? ?schemas-or-options)
                                    ?schemas (when-not options? ?schemas-or-options)
                                    options (when options? ?schemas-or-options)]
                                (inst ?all ?schemas options)))
  ([?all insts options] (-inst (m/schema ?all options) insts)))

(defn schemas []
  {:all (-all-schema nil)})
