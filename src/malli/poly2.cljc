(ns malli.poly2
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

;;;


;; -abstract / -instantiate for locally nameless representation
;; See "I am not a number: I am a free variable" - Conor McBride and James McKinna

(defn -abstract [?schema nme options]
  (let [inner (fn [this s path options]
                (let [properties (m/properties s)
                      options (cond-> options
                                (::scope properties) (update ::abstract-index inc))
                      s (cond-> s
                          (:registry properties)
                          (-> (m/ast options)
                              (update :registry #(not-empty
                                                   (into {} (map (fn [[k ast]]
                                                                   (-> ast
                                                                       (m/from-ast options)
                                                                       (m/-walk this (conj path :registry k) options)
                                                                       (m/ast options))))
                                                         %)))
                              (m/from-ast options)))]
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [abstract-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::f (let [[id] children]
                          (if (= nme id)
                            (m/schema [::b abstract-index] options)
                            s))
                    ::m/val (first children)
                    s)))]
    (m/schema
      [:schema {::scope true}
       (inner
         (reify m/Walker
           (-accept [_ s path options] true)
           (-inner [this s path options] (inner this s path options))
           (-outer [_ schema path children options]
             (outer schema path children options)))
         (m/schema ?schema options)
         []
         (assoc options
                ::m/walk-refs false
                ::m/walk-schema-refs false
                ::m/walk-entry-vals true
                ::abstract-index 0))]
      options)))

(defn -abstract-many [s names options]
  (reduce (fn [s nme]
            (-abstract s nme options))
          s names))

(defn -instantiate [?scope to options]
  (let [to (m/schema to options)
        inner (fn [this s path options]
                (m/-walk s this path
                         (cond-> options
                           (::scope (m/-properties s)) (update ::instantiate-index inc))))
        outer (fn [s path children {::keys [instantiate-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::b (let [[id] children]
                          (if (= instantiate-index id)
                            to
                            s))
                    ::m/val (first children)
                    s)))
        s (m/schema ?scope options)
        _ (when-not (-> s m/-properties ::scope)
            (m/-fail! ::instantiate-non-scope {:schema s}))]
    (inner
      (reify m/Walker
        (-accept [_ s path options] true)
        (-inner [this s path options] (inner this s path options))
        (-outer [_ schema path children options]
          (outer schema path children options)))
      (first (m/-children s))
      []
      (assoc options
             ::m/walk-refs false
             ::m/walk-schema-refs false
             ::m/walk-entry-vals true
             ::instantiate-index 0))))

(defn -instantiate-many [s images options]
  (reduce (fn [s image]
            (-instantiate s image options))
          s (rseq images)))

(defn -all-names [s]
  (map #(nth % 0) (-bounds s)))

(defn -fv [?schema options]
  (let [fvs (atom #{})
        inner (fn [this s p options]
                (case (m/type s)
                  :all (m/-walk s this p (update options ::bound-tvs into (-all-names s)))
                  (m/-walk s this p options)))]
    (inner (reify m/Walker
             (-accept [_ s _ _] s)
             (-inner [this s p options]
               (inner this s p options))
             (-outer [_ s p c {::keys [bound-tvs] :as options}]
               (case (m/type s)
                 ::local (let [id (first c)]
                           ;;TODO when id is simple-symbol?
                           (when-not (contains? bound-tvs id)
                             (swap! fvs conj (assoc (m/-properties s) :id id))))
                 ;;TODO think harder about :..
                 nil)
               s))
           (m/schema ?schema options)
           []
           (assoc options
                  ::m/walk-refs false
                  ::m/walk-schema-refs false
                  ::m/walk-entry-vals true
                  ::bound-tvs #{}))
    @fvs))

;;TODO capture avoidance
(defn -subst-tv [?schema tv->schema options]
  (let [inner (fn [this s path options]
                (case (m/type s)
                  ;; TODO rename :all binder if name capturing will occur
                  :all (m/-walk s this path (update options ::tv->schema
                                                    (fn [tv->schema]
                                                      (let [shadowed (-all-names s)]
                                                        (apply dissoc tv->schema shadowed)))))
                  :.. (m/-fail! ::todo-subst-tv-for-dotted-schema)
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [tv->schema] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::local (let [[id] children]
                              (if-some [[_ v] (find tv->schema id)]
                                v
                                s))
                    ::m/val (first children)
                    s)))]
    (inner
      (reify m/Walker
        (-accept [_ s path options] true)
        (-inner [this s path options] (inner this s path options))
        (-outer [_ schema path children options]
          (outer schema path children options)))
      (m/schema ?schema options)
      []
      (assoc options
             ::m/walk-refs false
             ::m/walk-schema-refs false
             ::m/walk-entry-vals true
             ::tv->schema tv->schema))))


;;;


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

(defn -free-schema [{:keys [type] :or {type ::f}}]
  ^{:type ::m/into-schema}
  (reify
    m/AST
    (-from-ast [parent ast options] (m/-from-value-ast parent ast options))
    m/IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (m/-check-children! type properties children 1 1)
      (when-not (case type
                  ::f (-> children first simple-keyword?)
                  ::b (-> children first nat-int?))
        (m/-fail! ::free-should-have-simple-keyword {:children children}))
      (let [form (delay (case type
                          ::f (-> children first)
                          (m/-simple-form parent properties children identity options)))
            cache (m/-create-cache options)]
        ^{:type ::m/schema}
        (reify
          m/AST
          (-to-ast [this _] (m/-to-value-ast this))
          m/Schema
          (-validator [_] (m/-fail! ::cannot-validate-free))
          (-explainer [this path] (m/-fail! ::cannot-explain-free))
          (-parser [this] (m/-fail! ::cannot-parse-free))
          (-unparser [this] (m/-fail! ::cannot-unparse-free))
          (-transformer [this transformer method options]
            (m/-intercepting (m/-value-transformer transformer this method options)))
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          m/Cached
          (-cache [_] cache)
          m/LensSchema
          (-keep [_])
          (-get [_ _ default] default)
          (-set [this key _] (m/-fail! ::non-associative-schema {:schema this, :key key})))))))

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
  {:all (-all-schema nil)
   ::f (-free-schema {:type ::f})
   ::b (-free-schema {:type ::b})})

(comment
  (m/ast [:schema {:registry {::a :int}} ::a])
  (m/ast [:enum :foo :bar])
  )
