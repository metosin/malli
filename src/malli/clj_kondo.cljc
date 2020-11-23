(ns malli.clj-kondo
  (:require #?(:clj [clojure.java.io :as io])
            [malli.core :as m]))

(declare transform)

(defmulti accept (fn [name _schema _children _options] name) :default ::default)

(defmethod accept ::default [_ _ _ _] :any)
(defmethod accept 'any? [_ _ _ _] :any)
(defmethod accept 'some? [_ _ _ _] :any) ;;??
(defmethod accept 'number? [_ _ _ _] :number)
(defmethod accept 'integer? [_ _ _ _] :int)
(defmethod accept 'int? [_ _ _ _] :int)
(defmethod accept 'pos-int? [_ _ _ _] :pos-int)
(defmethod accept 'neg-int? [_ _ _ _] :neg-int)
(defmethod accept 'nat-int? [_ _ _ _] :nat-int)
(defmethod accept 'float? [_ _ _ _] :double)
(defmethod accept 'double? [_ _ _ _] :double)
(defmethod accept 'pos? [_ _ _ _] :pos-int)
(defmethod accept 'neg? [_ _ _ _] :neg-int)
(defmethod accept 'boolean? [_ _ _ _] :boolean)
(defmethod accept 'string? [_ _ _ _] :string)
(defmethod accept 'ident? [_ _ _ _] :symbol) ;;??
(defmethod accept 'simple-ident? [_ _ _ _] :symbol) ;;??
(defmethod accept 'qualified-ident? [_ _ _ _] :symbol) ;;??
(defmethod accept 'keyword? [_ _ _ _] :keyword)
(defmethod accept 'simple-keyword? [_ _ _ _] :keyword)
(defmethod accept 'qualified-keyword? [_ _ _ _] :keyword)
(defmethod accept 'symbol? [_ _ _ _] :symbol)
(defmethod accept 'simple-symbol? [_ _ _ _] :symbol)
(defmethod accept 'qualified-symbol? [_ _ _ _] :symbol)
(defmethod accept 'uuid? [_ _ _ _] :any) ;;??
(defmethod accept 'uri? [_ _ _ _] :any) ;;??
(defmethod accept 'decimal? [_ _ _ _] :double) ;;??
(defmethod accept 'inst? [_ _ _ _] :any) ;;??
(defmethod accept 'seqable? [_ _ _ _] :seqable)
(defmethod accept 'indexed? [_ _ _ _] :vector) ;;??
(defmethod accept 'map? [_ _ _ _] :map)
(defmethod accept 'vector? [_ _ _ _] :vector)
(defmethod accept 'list? [_ _ _ _] :list)
(defmethod accept 'seq? [_ _ _ _] :seq)
(defmethod accept 'char? [_ _ _ _] :char)
(defmethod accept 'set? [_ _ _ _] :set)
(defmethod accept 'nil? [_ _ _ _] :nil)
(defmethod accept 'false? [_ _ _ _] :boolean) ;;??
(defmethod accept 'true? [_ _ _ _] :boolean) ;;??
(defmethod accept 'zero? [_ _ _ _] :int) ;;??
#?(:clj (defmethod accept 'rational? [_ _ _ _] :double)) ;;??
(defmethod accept 'coll? [_ _ _ _] :coll)
(defmethod accept 'empty? [_ _ _ _] :seq) ;;??
(defmethod accept 'associative? [_ _ _ _] :associative)
(defmethod accept 'sequential? [_ _ _ _] :sequential)
(defmethod accept 'ratio? [_ _ _ _] :int) ;;??
(defmethod accept 'bytes? [_ _ _ _] :char-sequence) ;;??

(defmethod accept :> [_ _ _ _] :number) ;;??
(defmethod accept :>= [_ _ _ _] :number) ;;??
(defmethod accept :< [_ _ _ _] :number) ;;??
(defmethod accept :<= [_ _ _ _] :number) ;;??
(defmethod accept := [_ _ _ _] :any) ;;??
(defmethod accept :not= [_ _ _ _] :any) ;;??

(defmethod accept :and [_ _ _ _] :any) ;;??
(defmethod accept :or [_ _ _ _] :any) ;;??

(defmethod accept ::m/val [_ _ children _] (first children))
(defmethod accept :map [_ _ children _]
  (let [{req true opt false} (->> children (group-by (m/-comp not :optional second)))
        opt (apply array-map (mapcat (fn [[k _ s]] [k s]) opt))
        req (apply array-map (mapcat (fn [[k _ s]] [k s]) req))]
    (cond-> {:op :keys}, (seq opt) (assoc :opt opt), (seq req) (assoc :req req))))

(defmethod accept :multi [_ _ _ _] :any) ;;??
(defmethod accept :map-of [_ _ _ _] :map) ;;??
(defmethod accept :vector [_ _ _ _] :vector)
(defmethod accept :sequential [_ _ _ _] :sequential)
(defmethod accept :set [_ _ _ _] :set)
(defmethod accept :enum [_ _ _ _])
(defmethod accept :maybe [_ _ [child] _] (if (keyword? child) (keyword "nilable" (name child)) child))
(defmethod accept :tuple [_ _ children _] children)
(defmethod accept :re [_ _ _ _] :regex)
(defmethod accept :fn [_ _ _ _] :fn)

(defmethod accept :string [_ _ _ _] :string)
(defmethod accept :int [_ _ _ _] :int)
(defmethod accept :double [_ _ _ _] :double)

(defmethod accept :boolean [_ _ _ _] :boolean)
(defmethod accept :keyword [_ _ _ _] :keyword)
(defmethod accept :qualified-keyword [_ _ _ _] :keyword)
(defmethod accept :symbol [_ _ _ _] :symbol)
(defmethod accept :qualified-symbol [_ _ _ _] :symbol)
(defmethod accept :uuid [_ _ _ _] :any) ;;??

(defmethod accept :ref [_ _ _ _] :any) ;;??
(defmethod accept :schema [_ schema _ options] (transform (m/deref schema) options))
(defmethod accept ::m/schema [_ schema _ options] (transform (m/deref schema) options))

(defmethod accept :merge [_ schema _ options] (transform (m/deref schema) options))
(defmethod accept :union [_ schema _ options] (transform (m/deref schema) options))
(defmethod accept :select-keys [_ schema _ options] (transform (m/deref schema) options))

(defn- -walk [schema _ children options] (accept (m/type schema) schema children options))

(defn -transform [?schema options] (m/walk ?schema -walk options))

;;
;; public api
;;

(defn transform
  ([?schema]
   (transform ?schema nil))
  ([?schema options]
   (-transform ?schema options)))

#?(:clj
   (defn save! [config]
     (let [cfg-file (io/file ".clj-kondo" "configs" "malli" "config.edn")]
       (io/make-parents cfg-file)
       (spit cfg-file config)
       config)))

(defn from [{:keys [schema ns name]}]
  (let [ns-name (-> ns str symbol)
        schema (m/schema schema)]
    (assert (= :or (m/type schema)))
    (reduce
      (fn [acc schema]
        (let [[input return] (m/children schema)
              args (mapv transform (m/children input))
              ret (transform return)
              arity (count args)]
          (conj acc {:ns ns-name
                     :name name
                     :arity arity
                     :args args
                     :ret ret}))) [] (m/children schema))))

(defn collect
  ([] (collect nil))
  ([ns]
   (let [-collect (fn [k] (or (nil? ns) (= k (symbol (str ns)))))]
     (->> (for [[k vs] (m/=>schemas) :when (-collect k) [_ v] vs v (from v)] v)))))

(defn linter-config [xs]
  (reduce
    (fn [acc {:keys [ns name arity args ret]}]
      (assoc-in
        acc [:linters :type-mismatch :namespaces (symbol (str ns)) name :arities arity]
        {:args args, :ret ret}))
    {:lint-as {'malli.schema/defn 'schema.core/defn}} xs))

(defn emit! []
  (-> (collect) (linter-config) (save!))
  nil)
