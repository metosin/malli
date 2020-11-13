(ns malli.regex-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [malli.regex :as re]))


(s/conform (s/+ int?) [])
(re/parse (re/repeat 2 ##Inf (re/fn int?)) [1 2])

(re/parse (re/cat [:type (re/fn keyword?)]
                  [:props (re/? (re/fn map?))]
                  #_(re/* (re/fn any?)))
          [:p])

(re/parse (re/cat [:type (re/fn keyword?)]
                  [:props (re/? (re/fn map?))]
                  [:body (re/* (re/fn any?))])
          [:p "Hello, world of data"])

(re/parse (re/cat
            [:type (re/fn keyword?)]
            [:props (re/? (re/fn map?))]
            [:body (re/* (re/fn int?))]) [:div {} 1 2 3])

(s/* any?)

(comment
  (s/def ::hiccup (s/alt :node (s/cat :name keyword?
                                      :props (s/? (s/map-of keyword? any?))
                                      :children any? #_(s/* (s/spec ::hiccup)))
                         :privitive (s/alt :nil nil?
                                           :boolean boolean?
                                           :number number?
                                           :text string?)))

  (s/conform ::hiccup [:div {:class [:foo :bar]}
                       [:p "Hello, world of data"]])

  (mh/valid? hiccup-model [:div {:class [:foo :bar]}
                           [:p "Hello, world of data"]]))


(comment

  (def hiccup-model
    (h/let ['hiccup (h/alt [:node (h/in-vector (h/cat [:name (h/fn keyword?)]
                                                      [:props (h/? (h/map-of (h/fn keyword?) (h/fn any?)))]
                                                      [:children (h/* (h/not-inlined (h/ref 'hiccup)))]))]
                           [:primitive (h/alt [:nil (h/fn nil?)]
                                              [:boolean (h/fn boolean?)]
                                              [:number (h/fn number?)]
                                              [:text (h/fn string?)])])]
           (h/ref 'hiccup)))

  (def expextations
    [{:name "empty"
      :data []
      :result []
      :malli (re/* (re/fn int?))
      :spec (s/* int?)}

     {:name "selecting right alt"
      :data [1 "a" :b 3]
      :result {:1 1, :2 [:option3 {:1 "a", :2 :b}], :3 3}
      :malli (re/cat [:1 (re/fn int?)]
                     [:2 (re/alt [:option1 (re/fn string?)]
                                 [:option2 (re/fn keyword?)]
                                 [:option3 (re/cat [:1 (re/fn string?)]
                                                   [:2 (re/fn keyword?)])])]
                     [:3 (re/fn int?)])
      :spec (s/cat :1 int?
                   :2 (s/alt :option1 string?
                             :option2 keyword?
                             :option3 (s/cat :1 string?
                                             :2 keyword?))
                   :3 int?)}

     {:name "selecting right alt (anonymous)"
      :data [1 "a" :b 3]
      :result [1 [2 ["a" :b]] 3]
      :malli (re/cat (re/fn int?)
                     (re/alt (re/fn string?)
                             (re/fn keyword?)
                             (re/cat (re/fn string?)
                                     (re/fn keyword?)))
                     (re/fn int?))}])

  (doseq [{:keys [name data result malli spec]} expextations]
    (testing name
      (when malli (testing "- malli" (is (= result (re/parse malli data)))))
      (when spec (testing "- spec" (is (= result (s/conform spec data))))))))

(re/parse (re/* (re/fn int?)) [])
(re/parse (re/alt [:number (re/fn int?)]
                  [:sequence (re/cat [:string (re/fn string?)])]) 1)

(s/conform
  (s/cat :0 int?
         :1 (s/* int?)
         :2 (s/* int?)
         :3 int?)
  [1 2 3 4])

(s/conform (s/alt :number int?
                  :sequence (s/cat :string string?)) ["1"])

(require '[minimallist.helper :as h]
         '[minimallist.core :as mc])

(mc/describe
  (h/cat [:0 (h/fn int?)]
         [:1 (h/repeat 0 2 (h/fn int?))]
         [:2 (h/repeat 0 2 (h/fn int?))]
         [:3 (h/fn int?)])
  [1 2 3 4 5])

(require '[net.cgrand.seqexp :as se])

(se/exec (se/cat (se/as :1 (se/repeat 0 2 int?))
                 (se/as :2 (se/repeat 0 2 int?)))
         [1 2])


(let [test-data [;; fn
                 (re/fn #(= 1 %))
                 [[1] 1
                  2 nil]

                 ;; alt - not inside a sequence
                 (re/alt [:number (re/fn int?)]
                         [:sequence (re/cat (re/fn string?))])
                 [1 nil
                  ["1"] [:sequence ["1"]]
                  [1] [:number 1]
                  "1" nil]

                 ;; alt - inside a cat
                 (re/cat (re/fn int?)
                         (re/alt [:option1 (re/fn string?)]
                                 [:option2 (re/fn keyword?)]
                                 [:option3 (re/cat (re/fn string?)
                                                   (re/fn keyword?))])
                         (re/fn int?))
                 [[1 "2" 3] [1 [:option1 "2"] 3]
                  [1 :2 3] [1 [:option2 :2] 3]
                  [1 "a" :b 3] [1 [:option3 ["a" :b]] 3]
                  [1 ["a" :b] 3] nil]

                 ;; alt - inside a cat, but with :inline false on its cat entry
                 #_#_(re/cat (re/fn int?)
                             (re/alt [:option1 (re/fn string?)]
                                     [:option2 (re/fn keyword?)]
                                     [:option3 (re/not-inlined (re/cat (re/fn string?)
                                                                       (re/fn keyword?)))])
                             (re/fn int?))
                     [[1 "2" 3] [1 [:option1 "2"] 3]
                      [1 :2 3] [1 [:option2 :2] 3]
                      [1 "a" :b 3] nil
                      [1 ["a" :b] 3] [1 [:option3 ["a" :b]] 3]]

                 ;; cat of cat, the inner cat is implicitly inlined
                 (re/cat (re/fn int?)
                         (re/cat (re/fn int?)))
                 [[1 2] [1 [2]]
                  [1] nil
                  [1 [2]] nil
                  [1 2 3] nil]

                 ;; cat of cat, the inner cat is explicitly not inlined
                 #_#_(re/cat (re/fn int?)
                             (re/not-inlined (re/cat (re/fn int?))))
                     [[1 [2]] [1 [2]]
                      [1 '(2)] [1 [2]]
                      [1] nil
                      [1 2] nil
                      [1 [2] 3] nil]

                 ;; repeat - no collection type specified
                 (re/repeat 0 2 (re/fn int?))
                 [[] []
                  [1] [1]
                  [1 2] [1 2]
                  '() []
                  '(1) [1]
                  '(2 3) [2 3]
                  [1 2 3] nil
                  '(1 2 3) nil]

                 ;; repeat - min > 0
                 (re/repeat 2 3 (re/fn int?))
                 [[] nil
                  [1] nil
                  [1 2] [1 2]
                  [1 2 3] [1 2 3]
                  [1 2 3 4] nil]

                 ;; repeat - max = +Infinity
                 (re/repeat 2 ##Inf (re/fn int?))
                 [[] nil
                  [1] nil
                  [1 2] [1 2]
                  [1 2 3] [1 2 3]]

                 ;; repeat - of a cat
                 (re/repeat 1 2 (re/cat (re/fn int?)
                                        (re/fn string?)))
                 [[1 "a"] [[1 "a"]]
                  [1 "a" 2 "b"] [[1 "a"] [2 "b"]]
                  [] nil
                  [1] nil
                  [1 2] nil
                  [1 "a" 2 "b" 3 "c"] nil]

                 ;; repeat - of a cat with :inlined false
                 #_#_(re/repeat 1 2 (re/not-inlined (re/cat (re/fn int?)
                                                            (re/fn string?))))
                     [[[1 "a"]] [[1 "a"]]
                      [[1 "a"] [2 "b"]] [[1 "a"] [2 "b"]]
                      ['(1 "a") [2 "b"]] [[1 "a"] [2 "b"]]
                      [] nil
                      [1] nil
                      [1 2] nil
                      [1 "a"] nil
                      [1 "a" 2 "b"] nil
                      [1 "a" 2 "b" 3 "c"] nil]

                 ;; let / ref
                 #_#_(re/let ['pos-even? (re/and (re/fn pos-int?)
                                                 (re/fn even?))]
                             (re/ref 'pos-even?))
                     [0 nil
                      1 nil
                      2 2
                      3 nil
                      4 4]]]

  (doseq [[model data-description-pairs] (partition 2 test-data)]
    (doseq [[data description] (partition 2 data-description-pairs)]
      (is (= [data (re/parse model data)]
             [data description])))))

;;
;; spike lee
;;

(re/describe (re/repeat 2 3 (re/cat (re/repeat 1 2 (re/fn int?)))) [1 2 3 4])
; => {:result [{0 [1 2]} {0 [3 4]}], :rest []}

(re/describe (re/repeat 2 3 (re/cat (re/repeat 1 4 (re/fn int?)))) [1 2 3 4])
; => {:result [{0 [1 2 3]} {0 [4]}], :rest []}

(re/describe (re/repeat 1 ##Inf (re/cat (re/repeat 1 ##Inf (re/fn int?))
                                        (re/repeat 0 ##Inf (re/fn int?)))) [1 2 3 4])
; => {:result [{0 [1 2 3], 1 [4]}], :rest []}

(re/describe (re/cat (re/+ (re/fn int?))
                     (re/+ (re/fn string?))
                     (re/+ (re/fn int?))
                     (re/+ (re/fn string?)))
             [1 2 3 "4" "5" 6 7 8 9 "10"])
; => {:result {0 [1 2 3], 1 ["4" "5"], 2 [6 7 8 9], 3 ["10"]}, :rest []}

(let [re (re/repeat 1 ##Inf (re/cat (re/repeat 1 ##Inf (re/fn int?))
                                    (re/repeat 0 ##Inf (re/fn int?))))
      valid? (re/validator re)
      parse (re/parser re)
      data [1 2 3 4]]
  {:data data
   :valid (valid? data)
   :parsed (parse data)})

(let [re (re/* (re/cat (re/fn string?)
                       (re/fn keyword?)))
      parse (re/parser re)]
  (parse ["-server" :foo "-verbose" :true "-user" :joe]))

(require '[clojure.spec.alpha :as s])

(s/def ::config (s/* (s/cat :prop string?, :val (s/alt :s string? :b boolean?))))

(s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])
;[{:prop "-server", :val [:s "foo"]}
; {:prop "-verbose", :val [:b true]}
; {:prop "-user", :val [:s "joe"]}]

(re/parse
  (re/* (re/cat [:prop (re/fn string?)]
                [:val (re/alt [:s (re/fn string?)]
                              [:b (re/fn boolean?)])]))
  ["-server" "foo" "-verbose" true "-user" "joe"])
;[{:prop "-server", :val [:s "foo"]}
; {:prop "-verbose", :val [:b true]}
; {:prop "-user", :val [:s "joe"]}]

[:* [:cat*
     [:prop string?]
     [:val [:alt*
            [:s string?]
            [:b boolean?]]]]]

[:* [:cat string? [:alt string? boolean?]]]

(do

  (require '[malli.regex :as re])

  #_(re/parse
      [:* [:cat
           [:prop string?]
           [:val [:alt
                  [:s string?]
                  [:b boolean?]]]]]
      ["-server" "foo" "-verbose" true "-user" "joe"])

  (re/parse
    (re/* (re/cat [:prop (re/fn string?)]
                  [:val (re/alt [:s (re/fn string?)]
                                [:b (re/fn boolean?)])]))
    ["-server" "foo" "-verbose" true "-user" "joe"])
  ;[{:prop "-server", :val [:s "foo"]}
  ; {:prop "-verbose", :val [:b true]}
  ; {:prop "-user", :val [:s "joe"]}]

  )

(s/explain-data (s/cat :a (s/map-of int? (s/cat :first int?))) [{1 ["1"]}])
(get-in [{1 ["1"]}] [0 1])
