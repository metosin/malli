(ns malli.constraint.string
  (:require [malli.constraint.util :refer [composite-constraint-types
                                           -add-gen-key
                                           -generator-types]]))

(def ^:private string-constraints
  (let [constraint-types (into {} (map (juxt identity identity))
                               (concat composite-constraint-types #{:max
                                                                    :min
                                                                    :re
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
                                                                    :palindrome
                                                                    :trim
                                                                    :triml
                                                                    :trimr
                                                                    :trim-newline
                                                                    :blank
                                                                    :non-blank
                                                                    #_:starts-with
                                                                    #_:ends-with
                                                                    #_:upper-case
                                                                    #_:lower-case
                                                                    #_:capitalized
                                                                    #_[:lines [:and [:< 1] [:<= 10]]]
                                                                    #_[:splits #"foo" [:and
                                                                                       [:max-count 1]
                                                                                       [:min-count 10]]]
                                                                    #_[:includes "foo"]
                                                                    #_[:gen/escapes {\a "__a__"}]
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
                                              :alphanumeric :alphanumeric-string
                                              :non-alphanumeric :non-alphanumeric-string
                                              :numeric :numeric-string
                                              :non-numeric :non-numeric-string
                                              :alpha :alpha-string
                                              :non-alpha :non-alpha-string
                                              :trim :trim-string
                                              :triml :triml-string
                                              :trimr :trimr-string
                                              :trim-newline :trim-newline-string
                                              :blank :blank-string
                                              :non-blank :non-blank-string
                                              :re :re-string))]
    {:flat-property-keys (into #{} (mapcat -add-gen-key)
                               #{:max
                                 :min
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
                                 :palindrome
                                 :trim
                                 :triml
                                 :trimr
                                 :trim-newline
                                 :blank
                                 :non-blank})
     :nested-property-keys (disj composite-constraint-types :not)
     :generator-constraint-types (into validator-constraint-types
                                       generator-constraint-types)
     :validator-constraint-types validator-constraint-types}))

(defn schema-constraints []
  {:string string-constraints})
