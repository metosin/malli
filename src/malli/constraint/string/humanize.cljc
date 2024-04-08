(ns malli.constraint.string.humanize
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.constraint.string.util :refer [code-point-offset-seq
                                                  code-point->string]]
            [malli.constraint.char :as mcc]
            [malli.core :as-alias m]))

(defn -msg-or-pred [msg pred]
  (fn [{:keys [value]} _options]
    (when-not (pred value)
      msg)))

(defn -msg-or-validates [msg]
  (fn [{:keys [validator value]} _options]
    (when-not (validator value)
      msg)))

(defn -msg-check-each [msg f]
  (fn [{:keys [value]} _options]
    (keep (fn [{:keys [code-point-offset
                       char-offset
                       code-point]}]
            (when-not (f code-point)
              (str msg ": "
                   "index "
                   char-offset
                   (when (not= code-point-offset char-offset)
                     (str " (code point offset " code-point-offset ")"))
                   " has "
                   (if (or ((some-fn mcc/printable-ascii? mcc/whitespace?)
                            code-point))
                     (pr-str (char code-point))
                     (str "code point " code-point))
                   ".")))
          (code-point-offset-seq value))))

(defn humanizers []
  {:alphanumeric-string (-msg-check-each "should be alphanumeric" mcc/alphanumeric?)
   [:not :alphanumeric-string] (-msg-or-validates "should contain a non-alphanumeric character")
   :non-alphanumeric-string (-msg-check-each "should not contain alphanumeric characters" (complement mcc/alphanumeric?))
   [:not :non-alphanumeric-string] (-msg-or-validates "should contain an alphanumeric character")

   :alpha-string (-msg-check-each "should be alphabetic" mcc/alpha?)
   [:not :alpha-string] (-msg-or-validates "should contain a non-alphabetic character")
   :non-alpha-string (-msg-check-each "should not contain alphabetic characters" (complement mcc/alpha?))
   [:not :non-alpha-string] (-msg-or-validates "should contain an alphabetic character")

   :numeric-string (-msg-check-each "should be numeric" mcc/numeric?)
   [:not :numeric-string] (-msg-or-validates "should contain a non-numeric character")
   :non-numeric-string (-msg-check-each "should not contain numeric characters" (complement mcc/numeric?))
   [:not :non-numeric-string] (-msg-or-validates "should contain a numeric character")
   
   :trim-string (fn [{:keys [value]} _options]
                  (cond-> []
                    (not= value (str/triml value)) (conj "should not have leading whitespace")
                    (not= value (str/trimr value)) (conj "should not have trailing whitespace")))
   [:not :trim-string] (-msg-or-pred "should have leading or trailing whitespace" #(not= % (str/trim %)))

   :triml-string (-msg-or-validates "should not have leading whitespace")
   [:not :triml-string] (-msg-or-validates "should have leading whitespace")
   :trimr-string (-msg-or-validates "should not have trailing whitespace")
   [:not :trimr-string] (-msg-or-validates "should have trailing whitespace")
   :trim-newline-string (-msg-or-validates "should not have trailing newline")
   [:not :trim-newline-string] (-msg-or-validates "should have trailing newline")
   :blank-string (-msg-or-validates "should be blank")
   [:not :blank-string] (-msg-or-validates "should not be blank")
   :non-blank-string (-msg-or-validates "should not be blank")
   [:not :non-blank-string] (-msg-or-validates "should be blank")
   :escapes-string (fn [{[_ m] :constraint :keys [value]} _]
                     (keep (fn [c]
                             (when (contains? m c)
                               (str "should escape character " (pr-str c))))
                           value))
   [:not :escapes-string] (fn [{[_ m] :constraint :keys [validator value]} _]
                            (when-not (validator value)
                              (str "should include at least one unescaped character:"
                                   (apply str (interleave " " (map pr-str (sort (keys m))))))))
   :includes-string (fn [{[_ s] :constraint :keys [validator value]} _]
                      (when-not (validator value)
                        (str "should include substring " (pr-str s))))
   [:not :includes-string] (fn [{[_ s] :constraint :keys [validator value]} _]
                             (when-not (validator value)
                               (str "should not include substring " (pr-str s))))
   :edn-string (fn [{[_ s] :constraint :keys [validator value humanize]}
                    {::m/keys [schema explain]}]
                 (assert (not (false? s)))
                 (when-not (validator value)
                   (let [eof (Object.)
                         opts {:eof eof}]
                     (if-some [[edn] (try [(edn/read-string opts value)]
                                          (catch Exception _))]
                       (when (and (some? s)
                                  (not (boolean? s)))
                         (when-some [errors (explain s edn)]
                           [(str "should be a string of " (pr-str s))
                            :constraint-failure (humanize errors)]))
                       "should be valid edn"))))
  [:not :edn-string] (fn [{[_ s] :constraint :keys [validator value humanize]}
                          {::m/keys [valid schema explain]}]
                       (prn "no edn")
                       (assert (not (false? s)))
                       (when (validator value)
                         (let [eof (Object.)
                               opts {:eof eof}]
                           (when-some [[edn] (try [(edn/read-string opts value)]
                                                  (catch Exception _))]
                             (if (and (some? s)
                                      (not (boolean? s)))
                               (when (valid s edn)
                                 (str "should not be a string of " (pr-str s)))
                               "should not be valid edn")))))
   })
