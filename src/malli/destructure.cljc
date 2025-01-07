(ns malli.destructure
  (:require [clojure.walk :as walk]
            [malli.core :as m]))

(defn -map-like? [x] (or (map? x) (and (seqable? x) (every? (fn [e] (and (vector? e) (= 2 (count e)))) x))))
(defn -qualified-key? [k] (and (qualified-keyword? k) (-> k name #{"keys" "syms"})))
(def MapLike (m/-collection-schema {:type 'MapLike, :empty {}, :pred -map-like?}))
(def Never (m/-simple-schema {:type 'Never, :pred (fn [_] false)}))

(defn -create [inline-schemas]
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Symbol" [:and symbol? [:not "Amp"]]
                "Separator" (if inline-schemas [:= :-] Never)
                "Map" [MapLike
                       [:or
                        [:tuple [:= :keys] [:vector ident?]]
                        [:tuple [:= :strs] [:vector ident?]]
                        [:tuple [:= :syms] [:vector ident?]]
                        [:tuple [:= :or] [:map-of simple-symbol? any?]]
                        [:tuple [:= :as] "Symbol"]
                        [:tuple [:fn -qualified-key?] [:vector ident?]]
                        [:tuple [:ref "ArgType"] any?]]]
                "Vector" [:catn
                          [:elems [:* "Arg"]]
                          [:rest [:? [:catn
                                      [:amp "Amp"]
                                      [:arg "Arg"]]]]
                          [:as [:? [:catn
                                    [:as "As"]
                                    [:sym "Symbol"]
                                    [:schema [:? [:catn
                                                  [:- "Separator"]
                                                  [:schema "Schema"]]]]]]]]
                "ArgType" [:orn
                           [:sym "Symbol"]
                           [:map "Map"]
                           [:vec [:schema [:ref "Vector"]]]]
                "Arg" [:alt
                       [:catn
                        [:arg "ArgType"]]
                       [:catn
                        [:arg "ArgType"]
                        [:- "Separator"]
                        [:schema "Schema"]]]
                "Binding" [:catn
                           [:elems [:* "Arg"]]
                           [:rest [:? [:catn
                                       [:amp "Amp"]
                                       [:arg "Arg"]]]]]}}
    "Binding"]))

(def Binding (-create false))
(def SchematizedBinding (-create true))

(declare -transform)

(defn -any? [x] (= :any x))
(defn -maybe? [x] (and (vector? x) (= :maybe (first x))))

(defn -vector [{{:keys [as elems rest]} :values} options]
  (or (some->> as :values :schema :values :schema (conj [:schema]))
      (let [ess (map #(let [s (-transform % options false)] (cond->> s (not (-maybe? s)) (conj [:?]))) elems)
            rs (if rest (-transform (:arg (:values rest)) options true) [:* :any])]
        [:maybe (if (seq ess) (-> [:cat] (into ess) (conj rs)) [:cat rs])])))

(defn -qualified-keys [m]
  (for [[k vs] m
        :when (-qualified-key? k)
        :let [f ({"keys" keyword, "syms" symbol} (name k))]
        :when f, v vs] (f (namespace k) (str v))))

(defn -keys [{:keys [keys strs syms] :as arg} {:keys [::references] :or {references true} :as options}]
  (let [any (fn [f ks] (map (fn [k] [(f k) :any]) ks))]
    (->> (concat (any keyword keys) (any str strs) (any identity syms)
                 (map (fn [k] [k (if (and references (qualified-keyword? k)) k :any)]) (-qualified-keys arg))
                 (map (fn [[k v]] [v (-transform (m/tags {:arg k}) options false)]) (filter #(m/tag? (key %)) arg)))
         (distinct))))

(defn -map [arg {:keys [::references ::required-keys ::closed-maps ::sequential-maps]
                 :or {references true, sequential-maps true} :as options} rest]
  (let [keys (-keys arg options)
        ->entry (fn [[k t]] (let [ref (and references (qualified-keyword? k))]
                              (cond (and ref required-keys) k
                                    required-keys [k t]
                                    :else (cond-> [k {:optional true}] (not ref) (conj t)))))
        ->arg (fn [[k t]] [:cat [:= k] (if (and references (qualified-keyword? k)) k t)])
        schema (cond-> [:map] closed-maps (conj {:closed true}) :always (into (map ->entry keys)))]
    (if (or rest sequential-maps)
      [:orn [:map schema] [:args (-> (into [:alt] (map ->arg) keys)
                                     (cond-> (not closed-maps) (conj [:cat [:not (into [:enum] (map first) keys)] :any]))
                                     (cond->> :always (conj [:*]) (not rest) (conj [:schema])))]]
      schema)))

(defn -transform [{{{k :key v :value} :arg schema :schema :as all} :values} options rest]
  (cond (and schema rest) (let [s (-transform all options false)] (if (-any? s) schema s))
        schema schema
        (= :vec k) (-vector v options)
        (= :map k) (-map v options rest)
        rest [:* :any]
        :else :any))

(defn -schema [{{:keys [elems rest]} :values} options]
  (cond-> :cat
    (or (seq elems) rest) (vector)
    (seq elems) (into (map #(-transform % options false) elems))
    rest (conj (-transform (:arg (:values rest)) options true))))

(defn -unschematize [x]
  (walk/prewalk #(cond-> % (and (map? %) (:- %)) (dissoc :- :schema)) x))

(defn -function-schema
  ([arglists] (-function-schema arglists nil))
  ([arglists options]
   (let [->schema (fn [arglist] [:=> (-schema (m/parse SchematizedBinding arglist) options) :any])]
     (as-> (map ->schema arglists) $ (if (next $) (into [:function] $) (first $))))))

;;
;; public api
;;

(defn parse
  "Takes a destructuring bindings vector (arglist)
   and returns a map with keys:

   | key            | description |
   | ---------------|-------------|
   | `:raw-arglist` | the original arglist (can have type-hints)
   | `:arglist`     | simplified clojure arglist (no type-hints)
   | `:schema`      | extracted malli schema
   | `:parsed`      | full parse results

   Parsing can be configured using the following options:

   | key                    | description |
   | -----------------------|-------------|
   | `::md/inline-schemas`  | support plumatic-style inline schemas (true)
   | `::md/sequential-maps` | support sequential maps in non-rest position (true)
   | `::md/references`      | qualified schema references used (true)
   | `::md/required-keys`   | destructured keys are required (false)
   | `::md/closed-maps`     | destructured maps are closed (false)

   Examples:

      (require '[malli.destructure :as md])

      (-> '[a b & cs] (md/parse) :schema)
      ; => [:cat :any :any [:* :any]]

      (-> '[a :- :string, b & cs :- [:* :int]] (md/parse) :schema)
      ; => [:cat :string :any [:* :int]]"
  ([arglist] (parse arglist nil))
  ([arglist {:keys [::inline-schemas] :or {inline-schemas true} :as options}]
   (let [parse-scheme (if inline-schemas SchematizedBinding Binding)
         parsed (m/parse parse-scheme arglist)
         arglist' (->> parsed (-unschematize) (m/unparse Binding))
         schema' (-schema parsed options)]
     (when (= ::m/invalid arglist') (m/-fail! ::invalid-arglist {:arglist arglist}))
     {:raw-arglist arglist, :parsed parsed, :arglist arglist', :schema schema'})))

(defn infer
  "Infers a schema from a function Var. Best effort."
  ([var] (infer var nil))
  ([var options] (-> var meta :arglists (-function-schema options))))
