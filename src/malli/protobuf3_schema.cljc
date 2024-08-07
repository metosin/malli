(ns malli.protobuf3-schema
  (:require [clojure.string :as str]))

(defn to-snake-case [s]
  (-> (name s)
      (str/replace #"([a-z0-9])([A-Z])" "$1_$2")
      (str/replace #"([A-Z]+)([A-Z][a-z])" "$1_$2")
      (str/replace #"-" "_")
      str/lower-case))

(defn to-pascal-case [s]
  (as-> s $
    (name $)
    (str/replace $ #"[-_\s]+" " ")
    (str/split $ #"\s+")
    (map str/capitalize $)
    (str/join $)))

(defn malli-type->protobuf-type [malli-type]
  (cond
    (= malli-type clojure.core/string?) "string"
    (= malli-type clojure.core/int?) "int32"
    (= malli-type clojure.core/boolean?) "bool"
    (= malli-type clojure.core/double?) "double"
    (= malli-type :string) "string"
    (= malli-type :int) "int32"
    (= malli-type :double) "double"
    (= malli-type :boolean) "bool"
    (= malli-type :keyword) "string"
    (= malli-type :symbol) "string"
    (= malli-type :uuid) "string"
    (= malli-type :uri) "string"
    (= malli-type :inst) "google.protobuf.Timestamp"
    (= malli-type :nil) "google.protobuf.NullValue"
    :else "bytes"))

(defn to-proto-name
  "Converts a Clojure-style name (possibly with hyphens) to a Protocol Buffer-compatible name."
  [s]
  (-> (name s)
      (str/replace "-" "_")))

(declare transform-schema)

(defn transform-map-schema [schema parent-name]
  (let [fields (rest schema)
        message-name (to-pascal-case parent-name)
        transformed-fields (map-indexed
                            (fn [idx [field-name field-schema]]
                              (let [field-type (transform-schema field-schema (to-pascal-case (name field-name)))]
                                {:name (to-snake-case field-name)
                                 :type field-type
                                 :index (inc idx)}))
                            fields)]
    {:type "message"
     :name message-name
     :fields transformed-fields}))

(defn transform-vector-schema [schema parent-name]
  (let [item-schema (second schema)
        item-type (transform-schema item-schema parent-name)]
    {:type "repeated"
     :value-type item-type}))

(defn transform-enum-schema [schema parent-name]
  (let [enum-name (to-pascal-case parent-name)
        values (drop 1 schema)]
    {:type "enum"
     :name enum-name
     :values (map-indexed
              (fn [idx value]
                {:name (-> (name value)
                           str/upper-case
                           to-proto-name)
                 :index idx})
              values)}))

(defn transform-schema [schema parent-name]
  (let [schema-type (if (vector? schema) (first schema) schema)]
    (case schema-type
      :map (transform-map-schema schema parent-name)
      :vector (transform-vector-schema schema parent-name)
      :set (transform-vector-schema schema parent-name)
      :enum (transform-enum-schema schema parent-name)

      (cond
        (fn? schema) {:name (malli-type->protobuf-type schema)}
        (keyword? schema) {:name (malli-type->protobuf-type schema)}
        :else {:name (malli-type->protobuf-type schema)}))))

(defn generate-field
  "Generate a Protocol Buffer field definition."
  [{:keys [type name index]}]
  (let [field-type (cond
                     (string? type) type
                     (map? type) (case (:type type)
                                   "repeated" (str "repeated " (if (map? (:value-type type))
                                                                 (:name (:value-type type))
                                                                 (:value-type type)))
                                   "enum" (:name type)
                                   (:name type))
                     :else (str type))]
    (str "  " field-type " " name " = " index ";")))

(defn generate-message
  "Generate a Protocol Buffer message definition."
  [{:keys [name fields]}]
  (str "message " name " {\n"
       (str/join "\n" (map generate-field fields))
       "\n}"))

(defn generate-enum
  "Generate a Protocol Buffer enum definition."
  [{:keys [name values]}]
  (str "enum " name " {\n"
       (str/join "\n" (map (fn [{:keys [name index]}]
                             (str "  " name " = " index ";"))
                           values))
       "\n}"))

(defn generate-definition
  "Generate a Protocol Buffer definition (message or enum)."
  [definition]
  (case (:type definition)
    "enum" (generate-enum definition)
    "message" (generate-message definition)))

(defn sort-definitions
  "Sort definitions to ensure proper order (enums first, then nested messages, then main message)."
  [definitions]
  (let [enums (filter #(= (:type %) "enum") definitions)
        messages (filter #(= (:type %) "message") definitions)
        main-message (first (filter #(= (:name %) "Message") messages))
        other-messages (remove #(= % main-message) messages)]
    (concat enums other-messages [main-message])))

(defn collect-definitions [schema]
  (letfn [(collect-helper [s acc]
            (if (map? s)
              (case (:type s)
                "message" (let [acc-with-message (conj acc s)]
                            (reduce (fn [acc field]
                                      (collect-helper (:type field) acc))
                                    acc-with-message
                                    (:fields s)))
                "repeated" (collect-helper (:value-type s) acc)
                "enum" (conj acc s)
                acc)
              acc))]
    (collect-helper schema [])))

(defn generate-protobuf3
  "Generate a complete Protocol Buffer 3 definition from a transformed schema."
  [transformed-schema]
  (let [all-definitions (collect-definitions transformed-schema)
        sorted-definitions (sort-definitions all-definitions)
        definitions-str (str/join "\n\n" (map generate-definition (drop-last sorted-definitions)))
        main-message (last sorted-definitions)]
    (str "syntax = \"proto3\";\n\n"
         definitions-str
         "\n\n"
         (generate-message (assoc main-message :name "Message")))))

(defn transform [schema]
  (let [transformed-schema (transform-schema schema "Message")]
    (generate-protobuf3 transformed-schema)))
