(ns malli.destructure
  (:require [malli.core :as m]
            [clojure.walk :as walk]))

(defn -map-like [x] (or (map? x) (and (seqable? x) (every? (fn [e] (and (vector? e) (= 2 (count e)))) x))))
(defn -keys-syms-key [k] (and (qualified-keyword? k) (-> k name #{"keys" "syms"})))
(def MapLike (m/-collection-schema {:type :map-like :empty {} :pred -map-like}))

(def Binding
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Local" [:and symbol? [:not "Amp"]]
                "Map" [MapLike
                       [:or
                        [:tuple [:= :keys] [:vector ident?]]
                        [:tuple [:= :strs] [:vector ident?]]
                        [:tuple [:= :syms] [:vector ident?]]
                        [:tuple [:= :or] [:map-of simple-symbol? any?]]
                        [:tuple [:= :as] "Local"]
                        [:tuple [:fn -keys-syms-key] [:vector ident?]]
                        [:tuple [:ref "Arg"] any?]]]
                "Vector" [:catn
                          [:elems [:* "Argument"]]
                          [:rest [:? [:catn
                                      [:amp "Amp"]
                                      [:arg "Argument"]]]]
                          [:as [:? [:catn
                                    [:as "As"]
                                    [:sym "Local"]]]]]
                "Arg" [:orn
                       [:sym "Local"]
                       [:map "Map"]
                       [:vec [:schema [:ref "Vector"]]]]
                "Argument" [:catn [:arg "Arg"]]
                "Binding" [:catn
                           [:elems [:* "Argument"]]
                           [:rest [:? [:catn
                                       [:amp "Amp"]
                                       [:arg "Argument"]]]]]}}
    "Binding"]))

(def SchematizedBinding
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Local" [:and symbol? [:not "Amp"]]
                "Separator" [:= :-]
                "Map" [MapLike
                       [:or
                        [:tuple [:= :keys] [:vector ident?]]
                        [:tuple [:= :strs] [:vector ident?]]
                        [:tuple [:= :syms] [:vector ident?]]
                        [:tuple [:= :or] [:map-of simple-symbol? any?]]
                        [:tuple [:= :as] "Local"]
                        [:tuple [:fn -keys-syms-key] [:vector ident?]]
                        [:tuple [:ref "Arg"] any?]]]
                "Vector" [:catn
                          [:elems [:* "Argument"]]
                          [:rest [:? [:catn
                                      [:amp "Amp"]
                                      [:arg "Argument"]]]]
                          [:as [:? [:catn
                                    [:as "As"]
                                    [:sym "Local"]
                                    [:schema [:? [:catn
                                                  [:- "Separator"]
                                                  [:schema "Schema"]]]]]]]]
                "Arg" [:orn
                       [:sym "Local"]
                       [:map "Map"]
                       [:vec [:schema [:ref "Vector"]]]]
                "Argument" [:alt
                            [:catn
                             [:arg "Arg"]]
                            [:catn
                             [:arg "Arg"]
                             [:- "Separator"]
                             [:schema "Schema"]]]
                "Binding" [:catn
                           [:elems [:* "Argument"]]
                           [:rest [:? [:catn
                                       [:amp "Amp"]
                                       [:arg "Argument"]]]]]}}
    "Binding"]))

(defn -any? [x] (= :any x))
(defn -maybe? [x] (and (vector? x) (= :maybe (first x))))

(defn -vector [{:keys [as elems rest]} options -transform]
  (or (some->> as :schema :schema (conj [:schema]))
      (let [ess (map #(let [s (-transform % options false)] (cond->> s (not (-maybe? s)) (conj [:?]))) elems)
            rs (if rest (-transform (:arg rest) options true) [:* :any])]
        [:maybe (if (seq ess) (-> [:cat] (into ess) (conj rs)) rs)])))

(defn -qualified [m]
  (for [[k vs] m
        :when (and (qualified-keyword? k) (-> k name #{"keys" "syms"}) (vector? vs) (every? ident? vs))
        :let [f ({"keys" keyword, "syms" symbol} (name k))]
        :when f
        v vs] (f (namespace k) (str v))))

(defn -keys [{:keys [keys strs syms] :as arg} {:keys [::references] :or {references true}}]
  (let [any (fn [f ks] (map (fn [k] [(f k) :any]) ks))]
    (->> (concat (any keyword keys) (any str strs) (any identity syms)
                 (map (fn [k] [k (if references k :any)]) (-qualified arg)))
         (distinct)
         (map first))))

(defn -map [arg {:keys [::references ::required-keys ::closed-maps ::sequential-maps] :or {sequential-maps true} :as options} rest]
  (let [keys (-keys arg options)
        ->entry (fn [k] (let [ref (and references (qualified-keyword? k))]
                          (cond (and ref required-keys) k
                                required-keys [k :any]
                                :else (cond-> [k {:optional true}] (not ref) (conj :any)))))
        ->arg (fn [k] [:cat [:= k] (if (and references (qualified-keyword? k)) k :any)])]
    (cond-> [:altn]
      :always (conj [:map (cond-> [:map] closed-maps (conj {:closed true}) :always (into (map ->entry keys)))])
      (or rest sequential-maps) (conj [:args (-> (into [:alt] (map ->arg keys))
                                                 (cond-> (not closed-maps) (conj [:cat :any :any]))
                                                 (cond->> :always (conj [:*]) (not rest) (conj [:schema])))]))))

(defn -transform [{[k v] :arg schema :schema :as all} options rest]
  (cond (and schema rest) (let [s (-transform all options false)] (if (-any? s) schema s))
        schema schema
        (= :vec k) (-vector v options -transform)
        (= :map k) (-map v options rest)
        rest [:* :any]
        :else :any))

(defn -unschematize [x]
  (walk/prewalk #(cond-> % (and (map? %) (:- %)) (dissoc :- :schema)) x))

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
   | `::md/required-keys`   | are destructured keys required (false)
   | `::md/closed-maps`     | are destructured maps closed (false)
   | `::md/references`      | are schema references used (false)

   Examples:

      (require '[malli.destructure :as md])

      (-> '[a b & cs] (md/parse) :schema)
      ; => [:cat :any :any [:* :any]]

      (-> '[a :- :string, b & cs :- [:* :int]] (md/parse) :schema)
      ; => [:cat :string :any [:* :int]]"
  ([arglist] (parse arglist nil))
  ([arglist {:keys [::inline-schemas] :or {inline-schemas true} :as options}]
   (let [parse-scheme (if inline-schemas SchematizedBinding Binding)
         {:keys [elems rest] :as parsed} (m/parse parse-scheme arglist)
         arglist' (->> parsed (-unschematize) (m/unparse Binding))
         schema' (cond-> :cat
                   (or (seq elems) rest) (vector)
                   (seq elems) (into (map #(-transform % options false) elems))
                   rest (conj (-transform (:arg rest) options true)))]
     (when (= ::m/invalid arglist') (m/-fail! ::invalid-arglist {:arglist arglist}))
     {:raw-arglist arglist, :parsed parsed, :arglist arglist', :schema schema'})))
