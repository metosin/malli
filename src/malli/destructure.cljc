(ns malli.destructure
  (:require [malli.core :as m]
            [clojure.walk :as walk]))

(defn -ns-keys [m]
  (->> (dissoc m :keys :strs :syms :or :as) (keys) (every? #(and (qualified-keyword? %) (-> % name #{"keys" "syms"})))))

(def Binding
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Local" [:and symbol? [:not "Amp"]]
                "Map" [:and
                       [:map
                        [:keys {:optional true} [:vector ident?]]
                        [:strs {:optional true} [:vector ident?]]
                        [:syms {:optional true} [:vector ident?]]
                        [:or {:optional true} [:map-of simple-symbol? any?]]
                        [:as {:optional true} "Local"]]
                       [:fn -ns-keys]]
                "Vector" [:catn
                          [:elems [:* "Argument"]]
                          [:rest [:? [:catn
                                      [:amp "Amp"]
                                      [:arg "Argument"]]]]
                          [:as [:? [:catn
                                    [:as "As"]
                                    [:sym "Local"]]]]]
                "Arg" [:alt
                       [:catn [:sym "Local"]]
                       [:catn [:map "Map"]]
                       [:catn [:vec [:schema [:ref "Vector"]]]]]
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
                "Map" [:and
                       [:map
                        [:keys {:optional true} [:vector ident?]]
                        [:strs {:optional true} [:vector ident?]]
                        [:syms {:optional true} [:vector ident?]]
                        [:or {:optional true} [:map-of simple-symbol? any?]]
                        [:as {:optional true} "Local"]]
                       [:fn -ns-keys]]
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
                "Arg" [:alt
                       [:catn [:sym "Local"]]
                       [:catn [:map "Map"]]
                       [:catn [:vec [:schema [:ref "Vector"]]]]]
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

(declare -transform)

(defn -any? [x] (= :any x))
(defn -maybe? [x] (and (vector? x) (= :maybe (first x))))

(defn -vector [{:keys [as elems rest]} options]
  (or (some->> as :schema :schema (conj [:schema]))
      (let [ess (map #(let [s (-transform % options false)] (cond->> s (not (-maybe? s)) (conj [:?]))) elems)
            rs (if rest (-transform (:arg rest) options true) [:* :any])]
        [:maybe (if (seq ess) (-> [:cat] (into ess) (conj rs)) rs)])))

(defn -qualified [m]
  (for [[k vs] m, :when (and (qualified-keyword? k) (-> k name #{"keys" "syms"}) (vector? vs) (every? ident? vs))
        :let [f ({"keys" keyword, "syms" symbol} (name k))], :when f, v vs] (f (namespace k) (str v))))

(defn -keys [{:keys [keys strs syms] :as arg}]
  (->> (distinct (-qualified arg)) (concat (map keyword keys) (map str strs) syms)))

(defn -map-args [arg rest]
  (let [keys (-keys arg)]
    [:altn
     [:map (into [:map] (map (fn [k] [k {:optional true} :any]) keys))]
     [:args (-> (into [:alt] (map (fn [k] [:cat [:= k] :any]) keys))
                (conj [:cat :any :any]) (->> (conj [:*]))
                (cond->> (not rest) (conj [:schema])))]]))

(defn -transform [{{:keys [vec map]} :arg schema :schema :as all} options rest]
  (cond (and schema rest) (let [s (-transform all options false)] (if (-any? s) schema s))
        schema schema
        vec (-vector vec options)
        map (-map-args map rest)
        rest [:* :any]
        :else :any))

(defn -unschematize [x]
  (walk/prewalk #(cond-> % (and (map? %) (:- %)) (dissoc :- :schema)) x))

(defn parse
  ([arglist] (parse arglist nil))
  ([arglist {::keys [schema] :or {schema Binding} :as options}]
   (let [{:keys [elems rest] :as parsed} (m/parse schema arglist)
         arglist' (->> parsed (-unschematize) (m/unparse Binding))
         schema' (cond-> :cat
                   (or (seq elems) rest) (vector)
                   (seq elems) (into (map #(-transform % options false) elems))
                   rest (conj (-transform (:arg rest) options true)))]
     (when (= ::m/invalid arglist') (m/-fail! ::invalid-arglist {:arglist arglist}))
     {:raw-arglist arglist, :parsed parsed, :arglist arglist', :schema schema'})))
