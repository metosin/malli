(ns malli.dev.virhe
  "initial code for https://github.com/metosin/virhe"
  (:require [arrangement.core]
            [fipp.edn]
            [fipp.ednize]
            [fipp.engine]
            [fipp.visit]
            #?(:clj [clojure.string :as str])))

;;
;; colors
;;

(def -dark-colors
  {:title 45
   :title-dark 32
   :text 253
   :link 255
   :string 180
   :constant 149
   :type 123
   :error 196})

(defn -color [color body printer]
  (let [colors (:colors printer -dark-colors)
        color (get colors color (:error colors))]
    #?(:cljs [:span body]
       :clj  [:span [:pass (str "\033[38;5;" color "m")] body [:pass "\u001B[0m"]])))

;;
;; EDN
;;

(defrecord EdnPrinter [symbols print-meta print-length print-level unknown]

  fipp.visit/IVisitor

  (visit-unknown [this x]
    (or (and unknown (try (some->> (unknown x) (fipp.visit/visit this))
                          (catch #?(:clj Exception, :cljs js/Error) _)))
        (fipp.visit/visit this (fipp.ednize/edn x))))

  (visit-nil [this]
    (-color :text "nil" this))

  (visit-boolean [this x]
    (-color :text (str x) this))

  (visit-string [this x]
    (-color :string (pr-str x) this))

  (visit-character [this x]
    (-color :text (pr-str x) this))

  (visit-symbol [this x]
    (-color :text (str x) this))

  (visit-keyword [this x]
    (-color :constant (pr-str x) this))

  (visit-number [this x]
    (-color :text (pr-str x) this))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (fipp.edn/pretty-coll this (-color :text "(" this) x :line (-color :text ")" this) fipp.visit/visit)))

  (visit-vector [this x]
    (fipp.edn/pretty-coll this (-color :text "[" this) x :line (-color :text "]" this) fipp.visit/visit))

  (visit-map [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank (first a) (first b))) x)]
      (fipp.edn/pretty-coll this (-color :text "{" this) xs [:span (-color :text "," this) :line] (-color :text "}" this)
                            (fn [printer [k v]]
                              [:span (fipp.visit/visit printer k) " " (fipp.visit/visit printer v)]))))

  (visit-set [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank a b)) (seq x))]
      (fipp.edn/pretty-coll this "#{" xs :line "}" fipp.visit/visit)))

  (visit-tagged [this {:keys [tag form]}]
    (let [object? (= 'object tag)
          tag-f (if (map? form) #(-color :type % this) identity)]
      [:group "#" (tag-f (pr-str tag))
       (when (or (and print-meta (meta form)) (not (coll? form))) " ")
       (if object?
         [:group [:align "[" (fipp.visit/visit this (last form))] "]"]
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

(defn -printer
  ([] (-printer nil))
  ([options]
   (let [defaults {:width 80
                   :symbols {}
                   :colors -dark-colors
                   :print-length *print-length*
                   :print-level *print-level*
                   :print-meta *print-meta*}]
     (map->EdnPrinter (cond->> options defaults (merge defaults))))))

(defn -pprint
  ([x] (-pprint x (-printer)))
  ([x printer]
   (let [printer (dissoc printer :margin)
         margin (apply str (take (:margin printer 0) (repeat " ")))]
     (binding [*print-meta* false]
       (fipp.engine/pprint-document [:group margin [:group (fipp.visit/visit printer x)]] printer)))))

(defn -print-doc [doc printer]
  (fipp.engine/pprint-document doc printer))

(defn -visit [x printer]
  (fipp.visit/visit printer x))

#?(:clj
   (defn -location [e ss]
     (let [start-with (fn [f s] (-> f first str (str/starts-with? s)))
           [target _ file line] (loop [[f :as fs] (-> e Throwable->map :trace), [s :as ss] ss]
                                  (cond (start-with f s) (recur (rest fs) ss)
                                        (seq (rest ss)) (recur fs (rest ss))
                                        :else f))]
       (try (let [file-name (str/replace file #"(.*?)\.\S[^\.]+" "$1")
                  target-name (name target)
                  ns (str (subs target-name 0 (or (str/index-of target-name (str file-name "$")) 0)) file-name)]
              (str ns ":" line))
            (catch Exception _)))))

(defn -title [message source {:keys [width] :as printer}]
  (let [between (- width (count message) 8 (count source))]
    [:group
     (-color :title-dark "-- " printer)
     (-color :title [:span message " "] printer)
     (-color :title-dark (apply str (take between (repeat "-"))) printer)
     (if source
       (-color :title [:span " " source " "] printer)
       (-color :title-dark "--" printer))
     (-color :title-dark "--" printer)]))

(defn -footer [{:keys [width] :as printer}]
  (-color :title-dark (apply str (take width (repeat "-"))) printer))

(defn -text [body printer]
  (-color :text body printer))

(defn -section [title location body printer]
  [:group
   (-title title location printer)
   :break :break
   body
   :break :break
   (-footer printer)])

;;
;; formatting
;;

(defmulti -format (fn [type _ _ _] type) :default ::default)

(defmethod -format ::default [_ message data printer]
  {:body (into [:group (-text (or (:message data) message) printer)] (when data [:break :break (-visit data printer)]))})

;;
;; documents
;;

(defn -exception-doc [e printer]
  (let [{:keys [type data]} (ex-data e)
        {:keys [title body] :or {title (:title printer)}} (-format type (ex-message e) data printer)
        location #?(:clj (-location e (:throwing-fn-top-level-ns-names printer)), :cljs nil)]
    (-section title location body printer)))
