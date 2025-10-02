# Reusable Schemas

Malli currently has two ways for re-using schemas (instances):

1. Schemas as Vars - *the [plumatic](https://github.com/plumatic/schema) way*
2. Schemas via Global Registry - *the [spec](https://clojure.org/about/spec) way*
3. Schemas via Local Registries

## Schemas as Vars

We can define Schemas using `def`:

```clojure
(require '[malli.core :as m])

(def UserId :uuid)

(def Address
  [:map
   [:street :string]
   [:lonlat [:tuple :double :double]]])

(def User
  [:map
   [:id UserId]
   [:name :string]
   [:address Address]])

(def user
  {:id (random-uuid)
   :name "Tiina"
   :address {:street "Satakunnunkatu 10"
             :lonlat [61.5014816, 23.7678986]}})

(m/validate User user)
;; => true
```

All subschemas as inlined as values:

```clojure
(m/schema User)
;[:map
; [:id :uuid]
; [:name :string]
; [:address [:map
;            [:street :string]
;            [:lonlat [:tuple :double :double]]]]]
```

## Schemas via Global Registry

To support spec-like mutable registry, we'll define the registry and a helper function to register a schema:

```clojure
(require '[malli.registry :as mr])

(defonce *registry (atom {}))

(defn register! [type ?schema]
  (swap! *registry assoc type ?schema))

(mr/set-default-registry!
 (mr/composite-registry
  (m/default-schemas)
  (mr/mutable-registry *registry)))
```

Registering Schemas:

```clojure
(register! ::user-id :uuid)

(register! ::address [:map
                      [:street :string]
                      [:lonlat [:tuple :double :double]]])

(register! ::user [:map
                   [:id ::user-id]
                   [:name :string]
                   [:address ::address]])

(m/validate ::user user)
;; => true
```

By default, reference keys are used instead of values:

```clojure
(m/schema ::user)
; :user/user
```

We can recursively deref the Schema to get the values:

```clojure
(m/deref-recursive ::user)
;[:map
; [:id :uuid]
; [:name :string]
; [:address [:map
;            [:street :string]
;            [:lonlat [:tuple :double :double]]]]]
```

### Decomplect Maps, Keys and Values

Clojure Spec declared [map specs should be of keysets only](https://clojure.org/about/spec#_map_specs_should_be_of_keysets_only). Malli supports this too:

```clojure
;; (╯°□°)╯︵ ┻━┻
(reset! *registry {})

(register! ::street :string)
(register! ::latlon [:tuple :double :double])
(register! ::address [:map ::street ::latlon])

(register! ::id :uuid)
(register! ::name :string)
(register! ::user [:map ::id ::name ::address])

(m/deref-recursive ::user)
;[:map
; [:user/id :uuid]
; [:user/name :string]
; [:user/address [:map
;                 [:user/street :string]
;                 [:user/latlon [:tuple :double :double]]]]]

;; data has a different shape now
(m/validate ::user {::id (random-uuid)
                    ::name "Maija"
                    ::address {::street "Kuninkaankatu 13"
                               ::latlon [61.5014816, 23.7678986]}})
;; => true
```

## Schemas via Local Registries

Schemas can be defined as a `ref->?schema` map:

```clojure
(def registry
  {::user-id :uuid
   ::address [:map
              [:street :string]
              [:lonlat [:tuple :double :double]]]
   ::user [:map
           [:id ::user-id]
           [:name :string]
           [:address ::address]]})
```

Using registry via Schema properties:

```clojure
(m/schema [:schema {:registry registry} ::user])
; => :user/user
```

Using registry via options:

```clojure
(m/schema ::user {:registry (merge (m/default-schemas) registry)})
```

Works with both:
<!-- :test-doc-blocks/skip -->
```clojure
(m/deref-recursive *1)
;[:map
; [:id :uuid]
; [:name :string]
; [:address [:map
;            [:street :string]
;            [:lonlat [:tuple :double :double]]]]]
```

# Which one should I use?

Here's a comparison matrix of the two different ways:

| Feature                          | Vars | Global Registry | Local Registry |
|----------------------------------|:----:|:---------------:|:--------------:|
| Supported by Malli               |  ✅   |        ✅        |       ✅        |
| Explicit require of Schemas      |  ✅   |        ❌        |       ✅        |
| Support Recursive Schemas        |  ✅   |        ✅        |       ✅        |
| Decomplect Maps, Keys and Values |  ❌   |        ✅        |       ✅        |

You should pick the way what works best for your project.

[My](https://gist.github.com/ikitommi) personal preference is the Var-style - it's simple and Plumatic proved it works well even with large codebases.

# Future Work

1. Could we also decomplect the Maps, Keys and Values with the Var Style?
2. Utilities for transforming between inlined and referenced models (why? why not!)

<!-- :test-doc-blocks/skip -->
```clojure
(-flatten-refs
 [:schema {:registry {::user-id :uuid
                      ::address [:map
                                 [:street :string]
                                 [:lonlat [:tuple :double :double]]]
                      ::user [:map
                              [:id ::user-id]
                              [:name :string]
                              [:address ::address]]}}
  ::user])
;[:map {:id :user/user}
; [:id [:uuid {:id :user/user-id}]]
; [:name :string]
; [:address [:map {:id :user/address}
;            [:street :string]
;            [:lonlat [:tuple :double :double]]]]]

(-unflatten-refs *1)
;[:schema {:registry {::user-id :uuid
;                     ::address [:map
;                                [:street :string]
;                                [:lonlat [:tuple :double :double]]]
;                     ::user [:map
;                             [:id ::user-id]
;                             [:name :string]
;                             [:address ::address]]}}
; ::user]
```
