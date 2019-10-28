(ns malli.scicloj)



;
; ███▄ ▄███▓ ▄▄▄       ██▓     ██▓     ██▓
; ▓██▒▀█▀ ██▒▒████▄    ▓██▒    ▓██▒    ▓██▒
; ▓██    ▓██░▒██  ▀█▄  ▒██░    ▒██░    ▒██▒
; ▒██    ▒██ ░██▄▄▄▄██ ▒██░    ▒██░    ░██░
; ▒██▒   ░██▒ ▓█   ▓██▒░██████▒░██████▒░██░
; ░ ▒░   ░  ░ ▒▒   ▓▒█░░ ▒░▓  ░░ ▒░▓  ░░▓
; ░  ░      ░  ▒   ▒▒ ░░ ░ ▒  ░░ ░ ▒  ░ ▒ ░
; ░      ░     ░   ▒     ░ ░     ░ ░    ▒ ░
; ░         ░  ░    ░  ░    ░  ░ ░
;

;
; Scicloj Meeting #9, Tommi Reiman
; Oct 28, 2019 06:00 PM in Universal Time UTC
;

;;
;; Malli
;;

; -> https://github.com/metosin/malli
; -> https://github.com/metosin/malli#motivation
; -> https://malli.io (the playground)

(require '[malli.core :as m])

(m/validate
  [:tuple string? boolean?] ; ?schema
  ["just data" true] ; value
  {})




















;;
;; Namespaces (cloc)
;;

;; the core (671)
'malli.core

;; value transformation: string, json (158)
'malli.transform

;; serialization (14)
'malli.edn

;; human errors (103)
'malli.error

;; value generation (91)
'malli.generator

;; infer schemas from examples (66)
'malli.provider

;; malli -> json-schema (83)
'malli.json-schema

;; malli -> swagger2 (21)
'malli.swagger






;; small core
;;  -> schema definition & validation
;;  -> hooks for extensions (-accept...)
;;  -> public apis
;;  -> only dependency is sci
;;
;; different concerns are in separate namespaces
;;  -> might have external dependencies



;; breadth-first to accumulate the emerged design,
;; still in a flux, might freeze soon





















;;
;; Schema AST, it's (almost) just data!
;;

;; core predicate (value & unqualified symbols via registry)
int?
'int?

;; schema element and args (hiccup)
[:tuple double? double?]

;; elements can have properties
[:tuple {:title "location"} double? double?]

;; composition
[:and int? [:> 18]]
[:or int? string?]

;; enums
[:enum "S" "M" "L"]

; maybe not?
[:maybe int?]

;; regexs
[:re #"kikka_.*"]

;; type-based shortcut
#"kikka_.*"

;; custom functions (not serializable)
[:fn (fn [x] (> x 10))]

;; custom portable functions (kudos to sci)
[:fn '(fn [x] (> x 10))]

;; collections
[:vector int?]
[:list int?]
[:set int?]

;; maps
[:map
 [:x int?]
 [:y int?]]

;; real life schemas might look like this
(m/schema
  [:and {:title "XY Map"}
   [:map
    [:x int?]
    [:y int?]
    [:address
     [:map
      [:street string?]
      [:city {:optional true} [:enum "HKI" "TRE"]]
      [:location [:tuple double? double?]]]]]
   [:fn {:error/message "x > y"}
    '(fn [{:keys [x y]}] (> x y))]])





















;;
;; AST gets parsed into reified Schema Protocols
;;

malli.core/IntoSchema
malli.core/Schema

;; -> performance & extendability

;; :maybe impl
'malli.core/-maybe-schema


















;;
;; Public functional API
;;


;; schema ast
(def ?schema
  [:and {:title "Age"} int? [:> 18]])

?schema

;; schema
(m/schema ?schema)

;; form
(m/form ?schema)

;; properties
(m/properties ?schema)

;; validate
(m/validate ?schema 1)
(m/validate ?schema 20)

;; optimized validate
(let [validate (m/validator ?schema)]
  (validate 1))

;; explain (thanks spec!)
(m/explain ?schema 1)
(m/explain ?schema 20)

;; optimized explain
(let [explain (m/explainer ?schema)]
  (explain 1))

;; accept (generic schema walker)
(m/accept ?schema (partial apply vector))
















;;
;; The Registry
;;

;; -> explicit, no mutable state
;; -> register your own domain schemas
;; -> create your

malli.core/default-registry

"https://github.com/metosin/malli#custom-registry"













;;
;; transforming values
;;

; one-way value transformation (like prismatic/schema coercion)
; coming: two way encode & decode, partial schema bijections
;  -> https://github.com/metosin/malli/pull/99

(require '[malli.transform :as mt])

;; JSON can represent numbers
(m/transform ?schema "1" mt/json-transformer)

;; String can't represent numbers
(m/transform ?schema "1" mt/string-transformer)















;;
;; humanized errors
;;

(require '[malli.error :as me])

; explain
(-> [:map
     [:x int?]
     [:y int?]]
    (m/explain {:x "1"}))

; with error messages
(-> [:map
     [:x int?]
     [:y int?]]
    (m/explain {:x "1"})
    (me/with-error-messages))

; output, for humans
(-> [:map
     [:x int?]
     [:y int?]]
    (m/explain {:x "1"})
    (me/humanize {:wrap :message}))

(def s-m-l
  [:enum {:error/message
          {:en "should be one of S, M or L"
           :fi "pitäisi olla S, M tai L"}}
   "S" "M" "L"])

(def age
  [:fn {:error/fn
        {:en '(fn [{:keys [value]} _]
                (str value ", should be > 18"))
         :fi '(fn [{:keys [value]} _]
                (str value ", pitäisi olla > 18"))}}
   '(fn [x] (and (int? x) (> x 18)))])

; extend like a boss!
(-> [:map
     [:id int?]
     [:size s-m-l]
     [:age age]]
    (m/explain {:size "XL", :age 10})
    (me/humanize
      {:locale :fi
       :wrap :message
       :errors (-> me/default-errors
                   (assoc-in
                     ['int? :error-message :fi]
                     "pitäisi olla numero")
                   (assoc-in
                     [::m/missing-key]
                     {:error/fn
                      {:en '(fn [{:keys [in]} _]
                              (str "missing key " (last in)))
                       :fi '(fn [{:keys [in]} _]
                              (str "puuttuu avain " (last in)))}}))}))















;;
;; Generating values (test.check & test.chuck)
;;

(require '[malli.generator :as mg])

;; random
(mg/generate keyword?)

;; using seed
(mg/generate [:enum "a" "b" "c"] {:seed 42})

;; using seed and size
(mg/generate pos-int? {:seed 10, :size 100})

;; regexs work too (currently not on ClojureScript)
(mg/generate
  [:re #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$"]
  {:size 10})

;; gen/elements (are not validated)
(mg/generate
  [:and {:gen/elements ["olipa" "kerran" "avaruus"]} string?])

;; portable gen/fmap
(mg/generate
  [:and {:gen/fmap '(partial str "kikka_")} string?]
  {:size 10})















;;
;; Persistent Schemas (sci & edamame)
;;

(require '[malli.edn :as edn])

(-> [:and
     [:map
      [:x int?]
      [:y int?]]
     [:fn {:error/message "x > y"}
      '(fn [{:keys [x y]}] (> x y))]]
    (edn/write-string)
    (edn/read-string)
    (m/explain {:x 3, :y 2}))















;;
;; Inferring Schemas from values (kudos to spec-provider)
;;

(require '[malli.provider :as mp])

(def samples
  [{:id "Lillan"
    :tags #{:artesan :coffee :hotel}
    :address {:street "Ahlmanintie 29"
              :city "Tampere"
              :zip 33100
              :lonlat [61.4858322, 23.7854658]}}
   {:id "Huber",
    :description "Beefy place"
    :tags #{:beef :wine :beer}
    :address {:street "Aleksis Kiven katu 13"
              :city "Tampere"
              :zip 33200
              :lonlat [61.4963599 23.7604916]}}])

(mp/provide samples)

;; all generated values are valid
(every? (partial m/validate (mp/provide samples)) samples)

;; Lots of room for improvement (see F# Type Providers)
;;  -> https://github.com/metosin/malli/issues/74
;;  -> pull Schemas from JSON Schema
;;  -> pull Schemas from Protocol Buffers
;;  -> ...













;;
;; Full Example
;;

(def Address
  [:map
   [:id string?]
   [:tags [:set keyword?]]
   [:address
    [:map
     [:street string?]
     [:city string?]
     [:zip int?]
     [:lonlat [:tuple double? double?]]]]])


;; success
(m/explain
  Address
  {:id "Lillan"
   :tags #{:artesan :coffee :hotel}
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322, 23.7854658]}})


;; with errors
(m/explain
  Address
  {:id "Lillan"
   :tags #{:artesan "coffee" :garden}
   :address {:street "Ahlmanintie 29"
             :zip 33100
             :lonlat [61.4858322, nil]}})


;; humanized errors
(-> Address
    (m/explain
      {:id "Lillan"
       :tags #{:artesan "coffee" :garden}
       :address {:street "Ahlmanintie 29"
                 :zip 33100
                 :lonlat [61.4858322, nil]}})
    (me/humanize
      {:wrap :message, :local :en}))


;; json->Address (:tags)
(m/transform
  Address
  {:id "Lillan",
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  mt/json-transformer)


;; composable transformers
(def strict-json-transformer
  (mt/transformer
    mt/strip-extra-keys-transformer
    mt/json-transformer))

(m/transform
  Address
  {:id "Lillan",
   :EVIL "LYN"
   :tags ["coffee" "artesan" "garden"],
   :address {:street "Ahlmanintie 29"
             :DARK "ORKO"
             :city "Tampere"
             :zip 33100
             :lonlat [61.4858322 23.7854658]}}
  strict-json-transformer)


;; schema merge (union)
(m/merge
  Address
  [:map
   [:description string?]
   [:address
    [:map
     [:country string?]]]])


;; sample values
(mg/sample Address)


;; infer schema from samples
(mp/provide (mg/sample Address))











;;
;; Walking over Schemas
;;

(defn visitor [schema childs _]
  {:name (m/name schema)
   :properties (or (m/properties schema) {})
   :childs childs})

(m/accept Address visitor)













;;
;; Malli -> JSON Schema
;;

(require '[malli.json-schema :as json-schema])

(json-schema/transform Address)
















;;
;; Malli -> Swagger Schema
;;

(require '[malli.swagger :as swagger])

(swagger/transform Address)











;;
;; Performance
;;

(require '[criterium.core :as cc])

(def json-pizza
  {:id 1
   :name "pizza"
   ;:toppings ["cheese" "onion"]
   :address {:street "Hämeenkatu 12"
             :location [61.4980359, 23.7643856]}})

(def Order
  [:map
   [:id int?]
   [:name string?]
   ;[:toppings [:set keyword?]]
   [:address
    [:map
     [:street string?]
     [:location double? double?]]]])

(def json->Order (m/transformer Order mt/json-transformer))



(json->Order json-pizza)
(cc/quick-bench (json->Order json-pizza))

; Space for improvement:
; -> boolean simplifier https://github.com/miikka/boolean-simplifier
; -> data-flow optimizations https://en.wikipedia.org/wiki/Optimizing_compiler#Data-flow_optimizations





;;
;; Programming with Schemas (TODO)
;;

(require '[schema.core :as s])
(require '[schema-tools.core :as st])

(def AddressSchema
  {:id s/Int
   :street s/Str
   (s/optional-key :city) s/Str
   (s/required-key :country) {:name s/Str}})

(st/select-keys AddressSchema [:street :city])
(st/dissoc AddressSchema :id :country)

(-> AddressSchema
    ;; allow extra keys on any level
    st/open-schema
    ;; all top-level keys optional
    st/optional-keys
    ;; all nested keys optional
    (st/update :country st/optional-keys))

; https://github.com/metosin/malli/pull/97






;;
;; Visualizing Schemas (TODO)
;;

;; good reference for plumatic schema
"https://github.com/metosin/schema-viz"










;;
;; Set operations (TODO)
;;

; union, intersection, difference

"https://github.com/metosin/malli/issues/82"

; I think this will be a killer feature!
; Taking intersection of Schema and value
; -> "what needs still to be collected"
;
; => toward fully dynamic data collection (forms, processses)








;;
;; Next steps?
;;

; finalize the core features and see the emerged design (optimize the whole!)
; roadmap as issues: https://github.com/metosin/malli/issues

; bubblin under:
; - spec-like conform & unform (like in spec)
; - regex/sequence schemas (like in spec)








;;
;; Compared to Plumatic Schema
;;




;;
;; Compared to clojure.spec
;;

(require '[clojure.spec-alpha2 :as spec2])

; different goal:
; -> spec: development time tools?
; -> malli: runtime tools






;;
;; malli features & spec counterparts
;;


; schemas as data & over-the-wire
; -> Spec2 should be good at this

"https://clojureverse.org/t/why-arent-specs-pure-data/4950/6"


; schema metadata:

"https://clojure.atlassian.net/browse/CLJ-2194"


; generator extensions, fn schemas and error fn as data

"-"

; spec value transformation:
; -> https://github.com/metosin/spec-tools

"-"

; spec walking:
; -> via https://github.com/metosin/spec-tools

"https://clojure.atlassian.net/browse/CLJ-2251"

; json-schema transformation:
; -> via https://github.com/metosin/spec-tools

; swagger2 transformation:
; -> via https://github.com/metosin/spec-tools

; inferring schema values:
; -> via https://github.com/stathissideris/spec-provider

; human errors:
; -> via https://github.com/alexanderkiel/phrase




;;
;; Spec Awesomeness, malli doesn't do (atm):
;;

; keysets

[:map ::x ::y [:x int?]]
[:keyset ::x ::y [:x int?]]

; regex-specs, should it?

(spec2/cat :k keyword? :ns (spec2/+ number?))

[:cat keyword? [:+ number?]]

; multi-specs, WIP

"https://github.com/metosin/malli/issues/94"

; conform & unform
; -> no named branches in malli
; -> would work with generated branches too

"maybe, if someone really needs those"

; function specs
; -> will use Plumatic Schema defn syntax
; -> Cursive will recognize these :)

(s/defn tuple :- [:tuple int? int?]
  "returns a tuple"
  [x :- int?, y :- int?]
  [x y])

(tuple 1 2)





;;
;; Syntax Example, unqualified keys
;;

(spec2/def ::order
  (spec2/schema
    {:purchaser string?
     :due-date inst?
     :line-items (spec2/coll-of
                   (spec2/schema
                     {:item-id pos-int?
                      :quantity nat-int?})
                   :kind vector?
                   :min-count 1
                   :gen-max 3)}))

(def order
  [:map
   [:purchaser string?]
   [:due-date inst?]
   [:line-items [:vector {:min 1, :gen/max 3}
                 [:map
                  [:item-id pos-int?]
                  [:quantity nat-int?]]]]])

; => ~same, malli has one syntax for everything

; => would be nice to backport some stuff to spec...









;
; ███▄ ▄███▓ ▄▄▄       ██▓     ██▓     ██▓
; ▓██▒▀█▀ ██▒▒████▄    ▓██▒    ▓██▒    ▓██▒
; ▓██    ▓██░▒██  ▀█▄  ▒██░    ▒██░    ▒██▒
; ▒██    ▒██ ░██▄▄▄▄██ ▒██░    ▒██░    ░██░
; ▒██▒   ░██▒ ▓█   ▓██▒░██████▒░██████▒░██░
; ░ ▒░   ░  ░ ▒▒   ▓▒█░░ ▒░▓  ░░ ▒░▓  ░░▓
; ░  ░      ░  ▒   ▒▒ ░░ ░ ▒  ░░ ░ ▒  ░ ▒ ░
; ░      ░     ░   ▒     ░ ░     ░ ░    ▒ ░
; ░         ░  ░    ░  ░    ░  ░ ░
;

;; Why is Malli?

"https://github.com/metosin/malli#motivation"

;; What is Malli?

"https://github.com/metosin/malli"

;; Playground

"https://malli.io"

;; new data-driven schema library from Clojure/Script

; - schemas and properties as data
; - validation & explain
; - schema serialization & de-serialization (without eval)
; - two-way value transformations
; - generating values
; - infer values from 3rd party sources (examples, ...)
; - tools for programming with schemas
; - no global state, explicit everything
; - first class error messages (both developers & end users)
; - highly configurable, "library for making schema libraries"?
; - targets JVM, Node, Browser & GraalVM
; - open for user & community feedback
; - and yes, should be fast too

"https://github.com/metosin/malli#links-and-thanks"

;;
;; Thanks for listening :)
;;

