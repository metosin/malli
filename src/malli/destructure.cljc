(ns malli.destructure
  (:require [malli.core :as m]
            [clojure.walk :as walk]))

(def Binding
  (m/schema
   [:schema
    {:registry {"Schema" any?
                "Amp" [:= '&]
                "As" [:= :as]
                "Local" [:and symbol? [:not "Amp"]]
                "Map" [:map
                       [:keys {:optional true} [:vector ident?]]
                       [:strs {:optional true} [:vector ident?]]
                       [:syms {:optional true} [:vector ident?]]
                       [:or {:optional true} [:map-of simple-symbol? any?]]
                       [:as {:optional true} "Local"]]
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
                "Map" [:map
                       [:keys {:optional true} [:vector ident?]]
                       [:strs {:optional true} [:vector ident?]]
                       [:syms {:optional true} [:vector ident?]]
                       [:or {:optional true} [:map-of simple-symbol? any?]]
                       [:as {:optional true} "Local"]]
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

(defn -args [{:keys [keys strs syms]}]
  (let [entry (fn [f] (fn [x] [:cat [:= (f x)] :any]))
        with (fn [acc ks f] (cond-> acc ks (into (map (entry f) ks))))]
    (-> [:alt] (with keys keyword) (with strs str) (with syms identity) (conj [:cat :any :any]) (->> (conj [:*])))))

(defn -map [{:keys [keys strs syms]}]
  (let [entry (fn [f] (fn [x] [(f x) {:optional true} :any]))
        with (fn [ks f acc] (cond-> acc ks (into (map (entry f) ks))))]
    (->> [:map] (with keys keyword) (with strs str) (with syms identity))))

(defn -map-args [arg rest]
  [:altn [:map (-map arg)] [:args (cond->> (-args arg) (not rest) (conj [:schema]))]])

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
