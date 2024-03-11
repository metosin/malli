(ns malli.dev.virhe
  "initial code for https://github.com/metosin/virhe"
  (:refer-clojure :exclude [format])
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
       :clj  (if color
               [:span [:pass (str "\033[38;5;" color "m")] body [:pass "\u001B[0m"]]
               [:span body]))))

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
     (map->EdnPrinter (cond-> defaults options (merge options))))))

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
  [:span (fipp.visit/visit printer x)])

#?(:clj
   (defn -location [e ss]
     (try
       (let [[target _ file line] (some (fn [[s :as line]] (if (not-any? #(str/starts-with? (str s) %) ss) line))
                                        (-> e Throwable->map :trace))]
         (let [file-name (str/replace file #"(.*?)\.\S[^\.]+" "$1")
               target-name (name target)
               ns (str (subs target-name 0 (or (str/index-of target-name (str file-name "$")) 0)) file-name)]
           (str ns ":" line)))
       (catch Exception _))))

#?(:clj
   (defn -hierarchy [^Class k]
     (loop [sk (.getSuperclass k), ks [k]]
       (if-not (= sk Object)
         (recur (.getSuperclass sk) (conj ks sk))
         ks))))

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
  [:group (-title title location printer) :break :break body :break :break (-footer printer)])

(defn -block [text body printer]
  [:group (-text text printer) :break :break
   (into [:align 2] (map (fn [x] (if (string? x) (-text x printer) x))
                         (if (sequential? body) body (vector body))))])

(defn -link [link printer]
  (-color :link link printer))

;;
;; formatting
;;

(defmulti -format (fn [e _ _] (-> e (ex-data) :type)) :default ::default)

(defmethod -format ::default [e data printer]
  (if-let [-format #?(:clj (some (methods -format) (-hierarchy (class e))), :cljs nil)]
    (-format e data printer)
    {:title "Unknown Error"
     :body [:group
            (-block "Type:" (-visit (type e) printer) printer) :break :break
            (-block "Message:" (-color :string (ex-message e) printer) printer)
            (when-let [data (ex-data e)]
              [:group :break :break (-block "Ex-data:" (-visit data printer) printer)])]}))

;;
;; public api
;;

(defn format [e printer]
  (-format e (-> e (ex-data) :data) printer))

(defn exception-document [e printer]
  (let [{:keys [title body] :or {title (:title printer)}} (format e printer)
        location #?(:clj (-location e (:throwing-fn-top-level-ns-names printer)), :cljs nil)]
    (-section title location body printer)))
