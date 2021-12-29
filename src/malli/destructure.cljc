(ns malli.destructure
  (:require [malli.core :as m]
            [clojure.walk :as walk]))

(def Bind
  (m/schema
   [:schema
    {:registry
     {"Schema" any?
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
              [:args [:* "SchematizedArgument"]]
              [:rest [:? [:catn
                          [:amp "Amp"]
                          [:arg "SchematizedArgument"]]]]]}}
    "Bind"]))

(declare transform)

(defn -maybe? [x] (and (vector? x) (= :maybe (first x))))

(defn -vector [{:keys [elems rest]}]
  (let [ess (map #(let [s (transform %)] (cond->> s (not (-maybe? s)) (conj [:?]))) elems)
        rs (if rest (transform (:arg rest) true) [:* :any])]
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

(defn transform
  ([x] (transform x false))
  ([{{:keys [vec map]} :arg schema :schema :as all} rest]
   (cond (and schema rest) [:and schema (transform (dissoc all :schema))]
         schema schema
         vec (-vector vec)
         map (if rest (-map-args map) (-map map))
         rest [:* :any]
         :else :any)))

(defn -unschematize [x]
  (walk/prewalk #(cond-> % (and (map? %) (:- %)) (dissoc :- :schema)) x))

(defn args [bind]
  (let [{:keys [args rest] :as parsed} (m/parse Bind bind)]
    {:bind bind
     :parsed parsed
     :schema (cond-> :cat
               (or (seq args) rest) (vector)
               (seq args) (into (map transform args))
               rest (conj (transform (:arg rest) true)))
     :arglist (->> parsed -unschematize (m/unparse Bind))}))

;; SPIKE
(require '[malli.dev.pretty :as pretty])
(defn -pp [x] (pretty/pprint x (pretty/-printer {:width 40})) (println))

(defn expext [expectations]
  (doseq [{:keys [name bind schema]} expectations]
    (when-not (= schema (:schema (args bind)))
      (-pp (args bind))
      (-pp {:name name})
      (-pp {:schema schema})
      (assert false))))

(expext
 [{:name "empty"
   :bind '[]
   :schema :cat}
  {:name "1 arg"
   :bind '[a]
   :schema [:cat :any]}
  {:name "2 args"
   :bind '[a b]
   :schema [:cat :any :any]}
  {:name "2 + varargs"
   :bind '[a b & cs]
   :schema [:cat :any :any [:* :any]]}
  {:name "sequence destructuring"
   :bind '[a [b1 [b2] & bs :as bss] & [c1 c2 & cs :as css]]
   :schema [:cat
            :any
            [:maybe
             [:cat
              [:? :any]
              [:maybe
               [:cat
                [:? :any]
                [:* :any]]]
              [:* :any]]]
            [:maybe
             [:cat
              [:? :any]
              [:? :any]
              [:* :any]]]]}
  {:name "map destructuring"
   :bind '[a {:keys [b c]
              :strs [e f]
              :syms [g h]
              :or {b 0, e 0, g 0} :as bc}]
   :schema [:cat
            :any
            [:map
             [:b {:optional true} :any]
             [:c {:optional true} :any]
             ["e" {:optional true} :any]
             ["f" {:optional true} :any]
             ['g {:optional true} :any]
             ['h {:optional true} :any]]]}
  {:name "Keyword argument functions now also accept maps"
   :bind '[& {:keys [a b], :strs [c d], :syms [e f] :as opts}]
   :schema [:cat
            [:altn
             [:map [:map
                    [:a {:optional true} :any]
                    [:b {:optional true} :any]
                    ["c" {:optional true} :any]
                    ["d" {:optional true} :any]
                    ['e {:optional true} :any]
                    ['f {:optional true} :any]]]
             [:args [:*
                     [:alt
                      [:cat [:= :a] :any]
                      [:cat [:= :b] :any]
                      [:cat [:= "c"] :any]
                      [:cat [:= "d"] :any]
                      [:cat [:= 'e] :any]
                      [:cat [:= 'f] :any]]]]]]}
  {:name "Nested Keyword argument"
   :bind '[[& {:keys [a b] :as opts}]
           & {:keys [a b] :as opts}]
   :schema [:cat
            [:maybe
             [:altn
              [:map [:map
                     [:a {:optional true} :any]
                     [:b {:optional true} :any]]]
              [:args [:* [:alt
                          [:cat [:= :a] :any]
                          [:cat [:= :b] :any]]]]]]
            [:altn
             [:map [:map
                    [:a {:optional true} :any]
                    [:b {:optional true} :any]]]
             [:args [:* [:alt
                         [:cat [:= :a] :any]
                         [:cat [:= :b] :any]]]]]]}])

#_(-pp (args '[[& {:keys [a b] :as opts}]
             & {:keys [a b] :as opts}]))

(-pp (args '[a [b & bs] & cs]))
#_(-pp (args '[& cs]))
#_(-pp (args '[a [b1 [b2] & bs :as bss] & [c1 c2 & cs :as css]]))

(defn destr [a [b & {:strs [c d] :as opts}]] [a b c d opts])
(destr 1 [2 "c" 1 :a 1 :a 2 :a 3])

(require '[malli.transform :as mt])
(m/validate [:map [:x :int]] [[:x 1]])
(m/decode [:map [:x :int]] {:x "1"} (mt/entry-transformer))
(m/decode [:map [:x :int]] [[:x "1"]] (mt/entry-transformer))

(m/parse
 [:cat
  :int
  [:* [:alt
       [:cat [:= :x] :int]
       [:cat [:= :y] :int]]]]
 [42 :x 1 :y 2 :y 1])

(m/parse
 [:altn
  [:map [:map
         [:a :int]
         [:b :int]]]
  [:args [:* [:alt
              [:cat [:= :a] :int]
              [:cat [:= :b] :int]]]]]
 #_[{:a 1, :b 2}]
 [:b 1 :a 2])

(m/parse
 [:cat
  [:altn
   [:map [:map
          [:a :any]
          [:b :any]]]
   [:args [:*
           [:alt
            [:cat [:= :a] :any]
            [:cat [:= :b] :any]]]]]]
 [:a 1 :b 2 :c 3])



[:cat
 [:maybe
  [:altn
   [:map [:map
          [:a {:optional true} :any]
          [:b {:optional true} :any]]]
   [:args [:* [:alt
               [:cat [:= :a] :any]
               [:cat [:= :b] :any]]]]]]
 [:altn
  [:map [:map
         [:a {:optional true} :any]
         [:b {:optional true} :any]]]
  [:args [:* [:alt
              [:cat [:= :a] :any]
              [:cat [:= :b] :any]]]]]]

(args '[& {:as m :keys [id before after]}])
;[:cat
; [:altn
;  [:map [:map
;         [:id {:optional true} :any]
;         [:before {:optional true} :any]
;         [:after {:optional true} :any]]]
;  [:args [:*
;          [:alt
;           [:cat [:= :id] :any]
;           [:cat [:= :before] :any]
;           [:cat [:= :after] :any]]]]]]
