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
| transformation chain    | transformers compose too: `(mt/transformer {:name :before} mt/json-transfomer {:name :after})`
