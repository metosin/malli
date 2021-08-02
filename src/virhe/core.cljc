(ns virhe.core
  (:require #?(:clj [clojure.string :as str])
            [arrangement.core]
            [fipp.visit]
            [fipp.edn]
            [fipp.engine]
            [fipp.ednize]))

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
  (defn- -colorz [color & text]
    (str "\033[38;5;" (colors color color) "m" (apply str text) "\u001B[0m"))

  (doseq [c (range 0 255)]
    (println (-colorz c "kikka") "->" c))

  (doseq [[n c] colors]
    (println (-colorz c "kikka") "->" c n)))

(defn -color [color & text]
  [:span
   [:pass (str "\033[38;5;" (colors color) "m")]
   (apply str text)
   [:pass "\u001B[0m"]])

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
    (-color :text "nil"))

  (visit-boolean [_ x]
    (-color :text (str x)))

  (visit-string [_ x]
    (-color :string (pr-str x)))

  (visit-character [_ x]
    (-color :text (pr-str x)))

  (visit-symbol [_ x]
    (-color :text (str x)))

  (visit-keyword [_ x]
    (-color :constant (pr-str x)))

  (visit-number [_ x]
    (-color :text (pr-str x)))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (fipp.edn/pretty-coll this (-color :text "(") x :line (-color :text ")") fipp.visit/visit)))

  (visit-vector [this x]
    (fipp.edn/pretty-coll this (-color :text "[") x :line (-color :text "]") fipp.visit/visit))

  (visit-map [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank (first a) (first b))) x)]
      (fipp.edn/pretty-coll this (-color :text "{") xs [:span (-color :text ",") :line] (-color :text "}")
                            (fn [printer [k v]]
                              [:span (fipp.visit/visit printer k) " " (fipp.visit/visit printer v)]))))

  (visit-set [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank a b)) x)]
      (fipp.edn/pretty-coll this "#{" xs :line "}" fipp.visit/visit)))

  (visit-tagged [this {:keys [tag form]}]
    (let [object? (= 'object tag)
          tag-f (if (map? form) (partial -color :type) identity)]
      [:group "#" (tag-f (pr-str tag))
       (when (or (and print-meta (meta form)) (not (coll? form)))
         " ")
       (if object?
         [:group
          [:align
           "["
           (-color :type (first form)) :line
           (-color :text (second form)) :line
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
  ([] (printer nil))
  ([options]
   (map->EdnPrinter
     (merge
       {:width 80
        :symbols {}
        :print-length *print-length*
        :print-level *print-level*
        :print-meta *print-meta*}
       options))))

(defn -pprint
  ([x] (-pprint x (printer)))
  ([x printer]
   (let [printer (dissoc printer :margin)
         margin (apply str (take (:margin printer 0) (repeat " ")))]
     (binding [*print-meta* false]
       (fipp.engine/pprint-document [:group margin [:group (fipp.visit/visit printer x)]] printer)))))

(defn -print-doc [doc printer]
  (fipp.engine/pprint-document doc {:width (:width printer)}))

(defn -visit [x printer]
  (fipp.visit/visit printer x))

(defn -location [e n]
  #?(:clj (let [[target _ file line] (-> (#'clojure.core/elide-top-frames e n) Throwable->map :trace first)]
            (try (if (not= 1 line)
                   (let [file-name (str/replace file #"(.*?)\.\S[^\.]+" "$1")
                         target-name (name target)
                         ns (str (subs target-name 0 (or (str/index-of target-name (str file-name "$")) 0)) file-name)]
                     (str ns ":" line)))
                 (catch Exception _)))))

(defn -title [message source {:keys [width]}]
  (let [between (- width (count message) 8 (count source))]
    [:group
     (-color :title-dark "-- ")
     (-color :title message " ")
     (-color :title-dark (apply str (take between (repeat "-"))))
     (if source
       (-color :title " " source " ")
       (-color :title-dark "--"))
     (-color :title-dark "--")]))

(defn -footer [{:keys [width]}]
  (-color :title-dark (apply str (take width (repeat "-")))))

(defn -text [& text]
  (apply -color :text text))

(defn -section [title location body printer]
  [:group
   (-title title location printer)
   :break :break
   body
   :break :break
   (-footer printer)])

(defmulti -format (fn [type _ _ _] type) :default ::default)

(defmethod -format ::default [_ message data printer]
  {:body (into [:group (-text (or (:message data) message))] (if data [:break :break (-visit data printer)]))})

;;
;; documents
;;

(defn -exception-doc [e printer]
  (let [{:keys [type data]} (ex-data e)
        {:keys [title body] :or {title (:title printer)}} (-format type (ex-message e) data printer)
        location (-location e (:throwing-fn-name printer))]
    (-section title location body printer)))

(defn -event-doc [type data printer]
  (let [{:keys [title body] :or {title (:title printer)}} (-format type (:message data) data printer)]
    (-section title nil body printer)))
