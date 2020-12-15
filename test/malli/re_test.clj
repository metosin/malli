(ns malli.re-test
  (:require [clojure.spec.alpha :as s]
            [net.cgrand.seqexp :as se]
            [minimallist.helper :as h]
            [minimallist.core :as mc]
            [net.cgrand.seqexp :as se]))

;; spec
[:cat]
[:alt]
[:?]
[:*]
[:+]

;; seqxp
[:*]
[:+]
[:?]
[:repeat]
[:*?]
[:+?]
[:??]
[:repeat?]
[:|]

;; malli
[:cat]
[:alt]
[:?]
[:*]
[:+]
[:repeat]

(se/exec
  (se/*
    (se/as :opts
           (se/cat
             (se/as :prop string?)
             (se/as :val (se/| (se/as :s string?) (se/as :b boolean?))))))
  ["-server" "foo" "-verbose" true "-user" "joe"])
;{:rest (),
; :match ("-server" "foo" "-verbose" true "-user" "joe"),
; :opts ("-user" "joe"),
; :prop ("-user"),
; :val ("joe"),
; :s ("joe"),
; :b (true)}


(s/conform
  (s/* (s/cat :prop string?
              :val (s/alt :s string?
                          :b boolean?)))
  ["-server" "foo" "-verbose" true "-user" "joe"])
;[{:prop "-server", :val [:s "foo"]}
; {:prop "-verbose", :val [:b true]}
; {:prop "-user", :val [:s "joe"]}]

(#'s/specize
  (s/* (s/cat :prop string?
              :val (s/alt :s string?
                          :b boolean?))))

(mc/describe
  (h/cat [:1 (h/* (h/cat [:prop (h/fn string?)]
                         [:val (h/alt [:s (h/fn string?)]
                                      [:b (h/fn boolean?)])]))]
         [:2 (h/+ (h/fn int?))])
  ["-server" "foo" "-verbose" true "-user" "joe" 1 2])
;[{:prop "-server", :val [:s "foo"]}
; {:prop "-verbose", :val [:b true]}
; {:prop "-user", :val [:s "joe"]}]

(h/* (h/cat [:prop (h/fn string?)]
            [:val (h/alt [:s (h/fn string?)]
                         [:b (h/fn boolean?)])]))

(mc/describe (h/repeat 2 3 (h/cat [0 (h/repeat 1 2 (h/fn int?))])) [1 2 3 4])
; => [{:ints [1 2]} {:ints [3]} {:ints [4]}]

(se/exec (se/repeat 2 3 (se/cat (se/repeat 1 2 int?))) [1 2 3 4])

(mc/describe (h/+ (h/cat [:ints (h/* (h/fn int?))]
                         [:ints (h/+ (h/fn int?))])) [1 2 3 4])
; => [{:ints [1]} {:ints [2]} {:ints [3]} {:ints [4]}]

(s/conform (s/+ (s/cat :ints (s/* int?)
                       :ints2 (s/+ int?))) [1 2 3 4])
; => [{:ints [1 2 3], :ints2 [4]}]


(mc/describe (h/+ (h/+ (h/fn int?))) [1 2 3]) ; => [[1] [2] [3]]
(s/conform (s/+ (s/+ int?)) [1 2 3]) ; => [[1 2 3]]

(s/conform (s/+ (s/cat :int int?)) [1 2 3]) ; => [{:int 1} {:int 2} {:int 3}]

(mc/valid?
  (h/cat [:key (h/fn string?)]
         [:value (h/fn string?)])
  #_["" "2"]
  {"" "2"})

(def Config [:* [:cat
                 [:prop string?]
                 [:val [:alt
                        [:s string?]
                        [:b boolean?]]]]])


(h/* (h/cat [:prop (h/fn string?)]
            [:val (h/alt [:s (h/fn string?)]
                         [:b (h/fn boolean?)])]))

;;
;; impl
;;

(require '[malli.core :as m])

(def registry m/default-schemas)

(m/validate [:* int?] [1 2 3] {:registry registry})

(mc/describe
  (h/cat [:head (h/repeat 1 3 (h/fn int?))]
         [:head2 (h/fn int?)]
         [:tail (h/* (h/fn int?))]
         [:tail2 (h/* (h/fn string?))])
  [1 2 3 4 5 "6" "7"])


(h/cat [:head (h/repeat 1 3 (h/fn int?))]
       [:head2 (h/fn int?)]
       [:tail (h/* (h/fn int?))]
       [:tail2 (h/* (h/fn string?))])

(mc/describe
  (h/repeat 1 3 (h/fn int?))
  [1 2 3])

(h/repeat 1 3 (h/fn int?))

(h/cat [:head (h/repeat 1 3 (h/fn int?))]
       [:head2 (h/fn int?)]
       [:tail (h/* (h/fn int?))]
       [:tail2 (h/* (h/fn string?))])

(s/conform (s/* (s/cat :ints (s/+ int?))) '(0 1))

(mc/describe
  (h/* (h/cat [:ints (h/+ (h/fn int?))]))
  '(0 1))

[:cat]
[:alt]
[:?]
[:*]
[:+]
[:repeat]


;;
;; impl
;;

#_
(impl
  #_(-repeat 2 4 string?)
  (-cat [(-repeat 2 2 string?)
         (-repeat 2 2 string?)])
  '("1" "2" "3" "4" "5" "6" 5))

;;
;; benchmark
;;

(mc/describe (h/* (h/cat [:ints (h/+ (h/fn int?))])) '(0 1))
(h/* (h/cat [:ints (h/+ (h/fn int?))]))

(se/exec
  (se/cat (se/as :1 (se/repeat 2 3 int?))
          (se/as :2 (se/repeat 2 3 int?))
          (se/as :3 (se/repeat 2 3 int?))
          (se/as :4 (se/repeat 2 3 int?)))
  (range 10))

(require '[malli.impl.regex :as mt])

#_
(mt/describe (mt/cat [(mt/+ (mt/fn int?))
                      (mt/+ (mt/fn string?))
                      (mt/+ (mt/fn int?))
                      (mt/+ (mt/fn string?))])
             [1 2 3 "4" "5" 6 7 8 9 "10"])

(comment
  (require '[criterium.core :as cc]))

(comment
  (let [data [1 2 3 "4" "5" 6 7 8 9 "10"]
        re (se/cat (se/as 1 (se/+ int?))
                   (se/as 2 (se/+ string?))
                   (se/as 3 (se/+ int?))
                   (se/as 4 (se/+ string?)))
        mre (h/cat [1 (h/+ (h/fn int?))]
                   [2 (h/+ (h/fn string?))]
                   [3 (h/+ (h/fn int?))]
                   [4 (h/+ (h/fn string?))])
        mtre [:cat
              [:+ int?]
              [:+ string?]
              [:+ int?]
              [:+ string?]]
        #_#_describer (mt/describer mtre)
        validator (m/validator mtre)
        sre (s/cat :1 (s/+ int?)
                   :2 (s/+ string?)
                   :3 (s/+ int?)
                   :4 (s/+ string?))]

    (println "\nMalli:")
    #_(prn (describer data))
    (prn (validator data))
    ;; 8.272667 µs
    #_(cc/quick-bench (describer data))
    ;; 6.777152 µs
    (cc/quick-bench (validator data))
    #_(time
        (dotimes [_ 10000]
          (mtre data)))

    (println "\nMinimallist:")
    (prn (mc/describe mre data))
    ;; 37.550816 µs
    (cc/quick-bench (mc/describe mre data))
    #_(time
        (dotimes [_ 10000]
          (mc/describe mre data)))

    (println "\nSeqexp:")
    (prn (se/exec re data))
    ;; 72.690279 µs
    (cc/quick-bench (se/exec re data))
    #_(time
        (dotimes [_ 10000]
          (se/exec re data)))

    (println "\nClojure.Spec:")
    (prn (s/conform sre data))
    ;; 103.550059 µs
    (cc/quick-bench (s/conform sre data))
    #_(time
        (dotimes [_ 10000]
          (s/conform sre data)))))

(let [data (doall (range 10))
      re (se/cat (se/as 1 (se/repeat 2 3 int?))
                 (se/as 2 (se/repeat 2 3 int?))
                 (se/as 3 (se/repeat 2 3 int?))
                 (se/as 4 (se/repeat 2 3 int?)))
      mre (h/cat [1 (h/repeat 2 3 (h/fn int?))]
                 [2 (h/repeat 2 3 (h/fn int?))]
                 [3 (h/repeat 2 3 (h/fn int?))]
                 [4 (h/repeat 2 3 (h/fn int?))])
      #_#_
      mtre (mt/cat [(mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))])
      sre (s/cat :ints (s/+ int?))]

  #_
  (let [describe (mt/describer mtre)]
    (println "\nMalli")
    (prn (describe data))
    (time
      (dotimes [_ 10000]
        (describe data))))

  (println "\nMinimallist")
  (prn (mc/describe mre data))
  (time
    (dotimes [_ 10000]
      (mc/describe mre data)))

  #_#_#_(println "\nClojure.Spec")
      (prn (s/conform sre data))
      (time
        (dotimes [_ 10000]
          (s/conform sre data)))

  (println "\nSeqexp")
  (prn (se/exec re data))
  (time
    (dotimes [_ 10000]
      (se/exec re data))))

(comment
  (let [re (se/* (se/cat (se/as :ints (se/+ int?))))
        mre (h/* (h/cat [:ints (h/+ (h/fn int?))]))
        sre (s/* (s/cat :ints (s/+ int?)))]

    (println "\nMinimallist")
    (prn (mc/describe mre '(0 1)))
    (time
      (dotimes [_ 10000]
        (mc/describe mre '(0 1))))

    (println "\nClojure.Spec")
    (prn (s/conform sre '(0 1)))
    (time
      (dotimes [_ 10000]
        (s/conform sre '(0 1))))

    (println "\nSeqexp")
    (prn (se/exec re '(0 1)))
    (time
      (dotimes [_ 10000]
        (se/exec re '(0 1))))))

(mc/describe
  (h/cat [:1 (h/repeat 2 3 (h/fn int?))]
         [:2 (h/repeat 2 3 (h/fn int?))]
         [:3 (h/repeat 2 3 (h/fn int?))]
         [:4 (h/repeat 2 3 (h/fn int?))])
  (range 12))

(h/cat [:1 (h/repeat 2 3 (h/fn int?))]
       [:2 (h/repeat 2 3 (h/fn int?))]
       [:3 (h/repeat 2 3 (h/fn int?))]
       [:4 (h/repeat 2 3 (h/fn int?))])

(#'mc/sequence-descriptions
  {}
  {:type :cat,
   :entries [{:key :1,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :2,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :3,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :4,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}]}

  (range 10))

#_(time
    (dotimes [_ 100000]
      (doall
        (#'mc/sequence-descriptions
          {}
          {:type :repeat,
           :min 2,
           :max 4,
           :elements-model {:type :fn,
                            :fn int?}}
          (range 10)))))

(defn sd [context model seq-data]
  (case (:type model)
    :fn [{:rest (next seq-data)
          :desc (first seq-data)}]
    :cat (reduce-kv (fn [seq-descriptions index entry]
                      (prn seq-descriptions)
                      (mapcat (fn [acc]
                                (->> (sd context (:model entry) (:rest acc))
                                     (map (fn [seq-description]
                                            {:rest (:rest seq-description)
                                             :desc (assoc (:desc acc) (:key entry index) (:desc seq-description))}))))
                              seq-descriptions))
                    [{:rest seq-data
                      :desc {}}]
                    (:entries model))
    :repeat (->> (iterate (fn [seq-descriptions]
                            (mapcat (fn [acc]
                                      (->> (sd context (:elements-model model) (:rest acc))
                                           (map (fn [seq-description]
                                                  {:rest (:rest seq-description)
                                                   :desc (conj (:desc acc) (:desc seq-description))}))))
                                    seq-descriptions))
                          [{:rest seq-data
                            :desc []}])
                 (take-while seq)
                 (take (inc (:max model))) ; inc because it includes the "match zero times"
                 (drop (:min model))
                 (reverse) ; longest repetitions first
                 (apply concat))))

(sd
  {}
  {:type :cat,
   :entries [{:key :1,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :2,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :3,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}
             {:key :4,
              :model {:type :repeat,
                      :min 2,
                      :max 3,
                      :elements-model {:type :fn,
                                       :fn int?}}}]}
  (range 10))

(s/cat :1 (s/+ int?)
       :2 (s/+ string?)
       :3 (s/+ int?)
       :4 (s/+ string?))

(let [data (doall (range 10))
      re (se/cat (se/as 1 (se/repeat 2 3 int?))
                 (se/as 2 (se/repeat 2 3 int?))
                 (se/as 3 (se/repeat 2 3 int?))
                 (se/as 4 (se/repeat 2 3 int?)))
      mre (h/cat [1 (h/repeat 2 3 (h/fn int?))]
                 [2 (h/repeat 2 3 (h/fn int?))]
                 [3 (h/repeat 2 3 (h/fn int?))]
                 [4 (h/repeat 2 3 (h/fn int?))])
      #_#_
      mtre (mt/cat [(mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))
                    (mt/repeat 2 3 (mt/fn int?))])
      sre (s/cat :ints (s/+ int?))]

  #_
  (let [describe (mt/describer mtre)]
    (println "\nMalli")
    (prn (describe data))
    (time
      (dotimes [_ 10000]
        (describe data)))))

;;
;;
;;

(require '[malli.impl.regex :as re])

(comment
  (let [data ["-server" "foo" "-verbose" true "-user" "joe"]]
    (doseq [[n f] [["seqexp" (partial
                               se/exec
                               (se/*
                                 (se/as :opts
                                        (se/cat
                                          (se/as :prop string?)
                                          (se/as :val (se/| (se/as :s string?) (se/as :b boolean?)))))))]
                   ["minimallist" (partial
                                    mc/describe
                                    (h/* (h/cat [:prop (h/fn string?)]
                                                [:val (h/alt [:s (h/fn string?)]
                                                             [:b (h/fn boolean?)])])))]
                   ["spec" (partial
                             s/conform
                             (s/* (s/cat :prop string?
                                         :val (s/alt :s string?
                                                     :b boolean?))))]
                   ["malli" (re/parser
                              (re/* (re/cat [:prop (re/fn string?)]
                                            [:val (re/alt [:s (re/fn string?)]
                                                          [:b (re/fn boolean?)])])))]]]
      (println n)
      (prn (f data))
      (cc/quick-bench (f data))
      (println)))

  (let [v (re/validator
            (re/* (re/cat [:prop (re/fn string?)]
                          [:val (re/alt [:s (re/fn string?)]
                                        [:b (re/fn boolean?)])])))]
    (cc/quick-bench (v ["-server" "foo" "-verbose" true "-user" "joe"]))))
;
;seqexp
;Execution time mean : 86.832562 µs
;
;spec
;Execution time mean : 39.515585 µs
;
;minimallist
;Execution time mean : 18.580211 µs
;
;malli
;Execution time mean : 6.191535 µs