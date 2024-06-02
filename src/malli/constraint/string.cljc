(ns malli.constraint.string
  (:require [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]))

(defn- string-constraints []
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max
                                                                    :min
                                                                    :max-code-points
                                                                    :min-code-points
                                                                    :alphanumeric
                                                                    :non-alphanumeric
                                                                    :letters
                                                                    :non-letters
                                                                    :numeric
                                                                    :non-numeric
                                                                    :alpha
                                                                    :non-alpha
                                                                    :sorted
                                                                    :distinct
                                                                    :distinct-code-points
                                                                    :palindrome
                                                                    :trim
                                                                    :triml
                                                                    :trimr
                                                                    :trim-newline
                                                                    :blank
                                                                    :non-blank
                                                                    ;;TODO :unescapes {"__MINUS__" \-}
                                                                    :escapes
                                                                    :includes
                                                                    :edn
                                                                    #_:re
                                                                    #_:starts-with
                                                                    #_:ends-with
                                                                    #_:upper-case
                                                                    #_:lower-case
                                                                    #_:capitalized
                                                                    #_[:lines [:and [:< 1] [:<= 10]]]
                                                                    #_[:splits #"foo" [:and
                                                                                       [:max-count 1]
                                                                                       [:min-count 10]]]
                                                                    #_[:index-of "foo" [:< 7]]
                                                                    #_[:last-index-of "foo" [:< 7]]
                                                                    #_:palindrome
                                                                    }))
        generator-constraint-types (-generator-types (keys constraint-types))
        validator-constraint-types (-> constraint-types
                                       ;; :gen/foo :=> :any
                                       (into (map (fn [c] [c :any])) (keys generator-constraint-types))
                                       (assoc :max :max-count
                                              :min :min-count
                                              :max-code-points :max-code-points-string
                                              :min-code-points :min-code-points-string
                                              :alphanumeric :alphanumeric-string
                                              :non-alphanumeric :non-alphanumeric-string
                                              :numeric :numeric-string
                                              :non-numeric :non-numeric-string
                                              :alpha :alpha-string
                                              :non-alpha :non-alpha-string
                                              :distinct :distinct-seqable
                                              :distinct-code-points :distinct-code-points-string
                                              :palindrome :palindrome-string
                                              :trim :trim-string
                                              :triml :triml-string
                                              :trimr :trimr-string
                                              :trim-newline :trim-newline-string
                                              :blank :blank-string
                                              :non-blank :non-blank-string
                                              :escapes :escapes-string
                                              :includes :includes-string
                                              :edn :edn-string
                                              ;;TODO
                                              ;:re :re-string
                                              ))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max
                                 :min
                                 :max-code-points
                                 :min-code-points
                                 :re
                                 :alphanumeric
                                 :non-alphanumeric
                                 :numeric
                                 :non-numeric
                                 :alpha
                                 :non-alpha
                                 :not
                                 :sorted
                                 :distinct
                                 :distinct-code-points
                                 :palindrome
                                 :trim
                                 :triml
                                 :trimr
                                 :trim-newline
                                 :blank
                                 :non-blank
                                 :escapes
                                 :includes
                                 :edn})
     :nested-property-keys (disj composite-constraint-types :not)
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)
     :validator-constraint-types validator-constraint-types}))

(defn schema-constraints []
  {:string (string-constraints)})
