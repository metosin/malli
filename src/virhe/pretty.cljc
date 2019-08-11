(ns virhe.pretty
  (:require [clojure.string :as str]
            [malli.exception :as exception]
            [arrangement.core]
            [fipp.visit]
            [fipp.edn]
            [fipp.ednize]
            [fipp.engine]))

;;
;; colors
;;

(def colors
  {:white 255
   :text 253
   :grey 245
   :title-dark 32
   :title 45
   :red 217

   :string 180
   :comment 243
   :doc 223
   :core-form 39
   :function-name 178
   :variable-name 85
   :constant 149
   :type 123
   :foreign 220
   :builtin 167
   :half-contrast 243
   :half-contrast-inverse 243
   :eldoc-varname 178
   :eldoc-separator 243
   :arglists 243
   :anchor 39
   :light-anchor 39
   :apropos-highlight 45
   :apropos-namespace 243
   :error 196})

(comment
  (defn- -color [color & text]
    (str "\033[38;5;" (colors color color) "m" (apply str text) "\u001B[0m"))

  (doseq [c (range 0 255)]
    (println (-color c "kikka") "->" c))

  (doseq [[n c] colors]
    (println (-color c "kikka") "->" c n))

  (doseq [[k v] expound.ansi/sgr-code]
    (println (expound.ansi/sgr "kikka" k) "->" k)))

(defn- -start [x] (str "\033[38;5;" x "m"))
(defn- -end [] "\u001B[0m")

(defn color [color & text]
  [:span
   [:pass (-start (colors color))]
   (apply str text)
   [:pass (-end)]])

;;
;; EDN
;;

(defrecord EdnPrinter [symbols print-meta print-length print-level unknown]

  fipp.visit/IVisitor

  (visit-unknown [this x]
    (fipp.visit/visit this (if unknown
                             (try
                               (unknown x)
                               (catch #?(:clj Exception, :cljs js/Error) _
                                 (fipp.ednize/edn x)))
                             (fipp.ednize/edn x))))

  (visit-nil [_]
    (color :text "nil"))

  (visit-boolean [_ x]
    (color :text (str x)))

  (visit-string [_ x]
    (color :string (pr-str x)))

  (visit-character [_ x]
    (color :text (pr-str x)))

  (visit-symbol [_ x]
    (color :text (str x)))

  (visit-keyword [_ x]
    (color :constant (pr-str x)))

  (visit-number [_ x]
    (color :text (pr-str x)))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (fipp.edn/pretty-coll this (color :text "(") x :line (color :text ")") fipp.visit/visit)))

  (visit-vector [this x]
    (fipp.edn/pretty-coll this (color :text "[") x :line (color :text "]") fipp.visit/visit))

  (visit-map [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank (first a) (first b))) x)]
      (fipp.edn/pretty-coll this (color :text "{") xs [:span (color :text ",") :line] (color :text "}")
                            (fn [printer [k v]]
                              [:span (fipp.visit/visit printer k) " " (fipp.visit/visit printer v)]))))

  (visit-set [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank a b)) x)]
      (fipp.edn/pretty-coll this "#{" xs :line "}" fipp.visit/visit)))

  (visit-tagged [this {:keys [tag form]}]
    (let [object? (= 'object tag)
          tag-f (if (map? form) (partial color :type) identity)]
      [:group "#" (tag-f (pr-str tag))
       (when (or (and print-meta (meta form)) (not (coll? form)))
         " ")
       (if object?
         [:group
          [:align
           "["
           (color :type (first form)) :line
           (color :text (second form)) :line
           (fipp.visit/visit this (last form))] "]"]
         (fipp.visit/visit this form))]))

  (visit-meta [this m x]
    (if print-meta
      [:align [:span "^" (fipp.visit/visit this m)] :line (fipp.visit/visit* this x)]
      (fipp.visit/visit* this x)))

  (visit-var [_ x]
    [:text (str x)])

  (visit-pattern [_ x]
    [:text (pr-str x)])

  (visit-record [this x]
    (fipp.visit/visit this (fipp.ednize/record->tagged x))))

(defn printer
  ([]
   (printer nil))
  ([options]
   (map->EdnPrinter
     (merge
       {:width 80
        :symbols {}
        :print-length *print-length*
        :print-level *print-level*
        :print-meta *print-meta*}
       options))))

(defn pprint
  ([x] (pprint x {}))
  ([x options]
   (let [printer (printer (dissoc options :margin))
         margin (apply str (take (:margin options 0) (repeat " ")))]
     (binding [*print-meta* false]
       (fipp.engine/pprint-document [:group margin [:group (fipp.visit/visit printer x)]] options)))))

(defn visit [printer x]
  (fipp.visit/visit printer x))

(defn print-doc [doc printer]
  (fipp.engine/pprint-document doc {:width (:width printer)}))

(defn repeat-str [s n]
  (apply str (take n (repeat s))))

;; TODO: this is hack, but seems to work and is safe.
(defn parse-source-str [[target _ file line]]
  (try
    (if (and (not= 1 line))
      (let [file-name (str/replace file #"(.*?)\.\S[^\.]+" "$1")
            target-name (name target)
            ns (str (subs target-name 0 (or (str/index-of target-name (str file-name "$")) 0)) file-name)]
        (str ns ":" line))
      "repl")
    (catch #?(:clj Exception, :cljs js/Error) _
      "unknown")))

(defn source-str [e]
  #?(:clj  (->> e Throwable->map :trace
                (drop-while #(not= (name (first %)) "reitit.core$router"))
                (drop-while #(= (name (first %)) "reitit.core$router"))
                next first parse-source-str)
     :cljs "unknown"))

(defn message-str [e]
  #?(:clj (.getMessage ^Exception e) :cljs (ex-message e)))

(defn title [message source {:keys [width]}]
  (let [between (- width (count message) 8 (count source))]
    [:group
     (color :title-dark "-- ")
     (color :title message " ")
     (color :title-dark (repeat-str "-" between) " ")
     (color :title source) " "
     (color :title-dark (str "--"))]))

(defn footer [{:keys [width]}]
  (color :title-dark (repeat-str "-" width)))

(defn text [& text]
  (apply color :text text))

(defn edn
  ([x] (edn x {}))
  ([x options]
   (str/trimr (with-out-str (pprint x options)))))

(defn exception-str [title-str message source printer]
  (with-out-str
    (print-doc
      [:group
       (title title-str source printer)
       [:break] [:break]
       message
       [:break] [:break]
       (footer printer)]
      printer)))

(defmulti format-exception (fn [type _ _] type))

(defn exception [title-str e]
  (let [data (-> e ex-data :data)
        message (format-exception (-> e ex-data :type) (message-str e) data)
        source (source-str e)]
    (ex-info (exception-str title-str message source (printer)) (assoc (or data {}) ::exception/cause e))))

(defn indent [x n]
  [:group (repeat-str " " n) [:align x]])

;;
;; Formatters
;;

(defmethod format-exception :default [_ message data]
  (prn data)
  (into [:group (text message)] (if data [[:break] [:break] (edn data)])))

#_(throw (exception "Model creation failed" (ex-info "kosh" {})))

#_(println
    (edn {:a 1} {:margin 3}))
