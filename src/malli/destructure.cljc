(ns malli.destructure
  (:require [malli.core :as m]
            [clojure.walk :as walk]))

(def Bind
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Local" [:and symbol? [:not "Amp"]]
                "Separator" [:= :-]
                "Map" [:map
                       [:keys {:optional true} [:vector ident?]]
                       [:strs {:optional true} [:vector ident?]]
                       [:syms {:optional true} [:vector ident?]]
                       [:or {:optional true} [:map-of simple-symbol? any?]]
                       [:as {:optional true} "Local"]]
                "Vector" [:catn
                          [:elems [:* "SchematizedArgument"]]
                          [:rest [:? [:catn
                                      [:amp "Amp"]
                                      [:arg "SchematizedArgument"]]]]
                          [:as [:? [:catn
                                    [:as "As"]
                                    [:sym "Local"]
                                    [:schema [:? [:catn
                                                  [:- "Separator"]
                                                  [:schema "Schema"]]]]]]]]
                "Argument" [:alt
                            [:catn [:sym "Local"]]
                            [:catn [:map "Map"]]
                            [:catn [:vec [:schema [:ref "Vector"]]]]]
                "SchematizedArgument" [:alt
                                       [:catn
                                        [:arg "Argument"]]
                                       [:catn
                                        [:arg "Argument"]
                                        [:- "Separator"]
                                        [:schema "Schema"]]]
                "Bind" [:catn
                        [:elems [:* "SchematizedArgument"]]
                        [:rest [:? [:catn
                                    [:amp "Amp"]
                                    [:arg "SchematizedArgument"]]]]]}}
    "Bind"]))

(declare -transform)

(defn -maybe? [x] (and (vector? x) (= :maybe (first x))))

(defn -vector [{:keys [elems rest]}]
  (let [ess (map #(let [s (-transform %)] (cond->> s (not (-maybe? s)) (conj [:?]))) elems)
        rs (if rest (-transform (:arg rest) true) [:* :any])]
    [:maybe (if (seq ess) (-> [:cat] (into ess) (conj rs)) rs)]))

(defn -args [{:keys [keys strs syms]}]
  (let [entry (fn [f] (fn [x] [:cat [:= (f x)] :any]))
        with (fn [ks f acc] (cond-> acc ks (into (map (entry f) ks))))]
    (->> [:alt] (with keys keyword) (with strs str) (with syms identity) (conj [:*]))))

(defn -map [{:keys [keys strs syms]}]
  (let [entry (fn [f] (fn [x] [(f x) {:optional true} :any]))
        with (fn [ks f acc] (cond-> acc ks (into (map (entry f) ks))))]
    (->> [:map] (with keys keyword) (with strs str) (with syms identity))))

(defn -map-args [arg] [:altn [:map (-map arg)] [:args (-args arg)]])

(defn -transform
  ([x] (-transform x false))
  ([{{:keys [vec map]} :arg schema :schema :as all} rest]
   (cond (and schema rest) [:and schema (-transform (dissoc all :schema))]
         schema schema
         vec (-vector vec)
         map (if rest (-map-args map) (-map map))
         rest [:* :any]
         :else :any)))

(defn -unschematize [x]
  (walk/prewalk #(cond-> % (and (map? %) (:- %)) (dissoc :- :schema)) x))

(defn parse [bind]
  (let [{:keys [elems rest] :as parsed} (m/parse Bind bind)]
    {:bind bind
     :parsed parsed
     :schema (cond-> :cat
               (or (seq elems) rest) (vector)
               (seq elems) (into (map -transform elems))
               rest (conj (-transform (:arg rest) true)))
     :arglist (->> parsed -unschematize (m/unparse Bind))}))
