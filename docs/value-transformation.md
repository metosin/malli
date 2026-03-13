# Value Transformation

## Terminology

| Term                    | Description
|-------------------------|------------
| transformation function | a function of A->B, e.g. conversion strings to dates
| decoding                | a process of transforming (invalid) values into potentially valid ones (IN), `m/decoder` & `m/decode`
| encoding                | a process of transforming (valid) values into something else (OUT), `m/encoder` & `m/encode`
| transformer             | a top-level component that maps Schemas with transformation functions (e.g. “json-transformer transforms strings to dates, but not strings to numbers”). Needed in encoding and decoding, `mt/transformer`
| named transformer       | If a transformer has `:name` defined, Schemas can define their transformation functions (for both encoding & decoding) using Schema properties
| interceptor             | a component that bundles transforming functions into *transforming phases*
| transforming phase      | either `:enter` or `:leave`, timing when a transformation function is applied in the chain (before or after the fact)
| interceptor chain       | a sequence of interceptors that is used to run the (optimized sequence of) transformation functions from interceptors in correct order
| transformation chain    | transformers compose too: `(mt/transformer {:name :before} mt/json-transformer {:name :after})`

## Defining custom transformers

### Option A: define the transformer for each schema

```clj
(require '[malli.core :as m])
(require '[malli.transform :as mt])
(def ints-as-strings-transformer
  (mt/transformer {:name :ints-as-strings
                   :encoders {:int #(apply str (replicate % \#))}
                   :decoders {:int #(if (string? %) (.length %) %)}}))
(m/encode [:map [:count :int]] {:count 4} ints-as-strings-transformer)
; => {:count "####"}
(m/decode [:map [:count :int]] {:count "#####"} ints-as-strings-transformer)
; => {:count 5}
```

### Option B: define transformers in properties

Using a named transformer lets you distribute the transformer definition to the schema definitions.

```clj
(require '[malli.core :as m])
(require '[malli.transform :as mt])
(require '[clojure.string :as str])
(def my-schema
  [:map
   [:capitalized [:string {:decode/custom str/upper-case
                           :encode/custom str/lower-case}]]
   [:trimmed [:string {:decode/custom str/trim}]]])
(m/decode my-schema {:capitalized "xyz" :trimmed " abc "} (mt/transformer {:name :custom}))
; => {:capitalized "XYZ", :trimmed "abc"}
(m/encode my-schema {:capitalized "XYZ" :trimmed " abc "} (mt/transformer {:name :custom}))
; => {:capitalized "xyz", :trimmed " abc "}
```
