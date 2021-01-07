(ns malli.impl.regex
  "Regular expressions of sequences implementation namespace.

  The implementation is very similar to Packrat or GLL parser combinators.
  The parsing functions need to be written in CPS to support backtracking
  inside :*, :+ and :repeat. They also need to be trampolined because the
  (manually) CPS-converted code (for :*, :+ and :repeat) has to use tail
  calls instead of loops and Clojure does not have TCO.

  Because backtracking is used we need to memoize (parsing function, seq
  position, register stack) triples to avoid exponential behaviour. Discarding
  the memoization tables after traversing an input seq also requires trampolining.
  Because regular expressions don't use (nontail) recursion by definition, finding
  a memoization entry just means the parser already went 'here' and ultimately
  failed; much simpler than the graph-structured stacks of GLL. And the register
  stack is only there for and used by :repeat.

  https://epsil.github.io/gll/ is a nice explanation of GLL parser combinators
  and has links to papers etc. It also inspired Instaparse, which Engelberg
  had a presentation about at Clojure/West 2014.

  Despite the CPS and memoization, this implementation looks more like normal
  Clojure code than the 'Pike VM' in Seqexp. Hopefully JITs also see it that
  way and compile decent machine code for it. It is also much easier to extend
  for actual parsing (e.g. encode, decode [and conform?]) instead of just
  recognition for `validate`."

  (:refer-clojure :exclude [+ * repeat cat])
  (:require [malli.impl.error :refer [-fail! -error]])
  #?(:clj (:import [clojure.lang MapEntry]
                   [java.util ArrayDeque HashSet])))

;;;; # Utils (TODO: move them out of here)

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))

;;;; # Driver Protocols

(defprotocol ^:private Driver
  (succeed! [self])
  (succeeded? [self])
  (pop-thunk! [self]))

(defprotocol ^:private IValidationDriver
  (-park-validator! [driver validator regs pos coll k])
  (park-validator! [driver validator regs pos coll k]))

(defprotocol ^:private IExplanationDriver
  (-park-explainer! [driver explainer regs pos coll k])
  (park-explainer! [driver explainer regs pos coll k])
  (value-path [self pos])
  (fail! [self pos errors*])
  (latest-errors [self]))

(defprotocol ^:private IParseDriver
  (-park-transformer! [driver transformer regs coll* pos coll k])
  (park-transformer! [driver transformer regs coll* pos coll k])
  (succeed-with! [self v])
  (success-result [self]))

;;;; # Primitives

;;;; ## Seq Item

(defn item-validator [valid?]
  (fn [_ _ pos coll k]
    (when (and (seq coll) (valid? (first coll)))
      (k (inc pos) (rest coll)))))

(defn item-explainer [path schema schema-explainer]
  (fn [driver _ pos coll k]
    (let [in (value-path driver pos)]
      (if (seq coll)
        (let [errors (schema-explainer (first coll) in [])]
          (if (seq errors)
            (fail! driver pos errors)
            (k (inc pos) (rest coll))))
        (fail! driver pos [(-error path in schema nil :malli.core/end-of-input)])))))

(defn item-parser [valid?]
  (fn [_ _ pos coll k]
    (when (seq coll)
      (let [v (first coll)]
        (when (valid? v)
          (k v (inc pos) (rest coll)))))))

(defn item-encoder [valid? encode]
  (fn [_ _ coll* pos coll k]
    (when (seq coll)
      (let [v (first coll)]
        (when (valid? v)
          (k (conj coll* (encode v)) (inc pos) (rest coll)))))))

(defn item-decoder [decode valid?]
  (fn [_ _ coll* pos coll k]
    (when (seq coll)
      (let [v (decode (first coll))]
        (when (valid? v)
          (k (conj coll* v) (inc pos) (rest coll)))))))

;;;; ## End of Seq

(defn end-validator [] (fn [_ _ pos coll k] (when (empty? coll) (k pos coll))))

(defn end-explainer [schema path]
  (fn [driver _ pos coll k]
    (if (empty? coll)
      (k pos coll)
      (fail! driver pos (list (-error path (value-path driver pos) schema (first coll) :malli.core/input-remaining))))))

(defn end-parser [] (fn [_ _ pos coll k] (when (empty? coll) (k nil pos coll))))

(defn end-transformer [] (fn [_ _ coll* pos coll k] (when (empty? coll) (k coll* pos coll))))

;;;; # Combinators

;;;; ## Catenation

(defn- entry->regex [?kr] (if (vector? ?kr) (get ?kr 1) ?kr))

(defn cat-validator
  ([] (fn [_ _ pos coll k] (k pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs pos coll k]
       (r driver regs pos coll (fn [pos coll] (r* driver regs pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-validator ?kr (reduce (fn [acc ?kr] (cat-validator ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn cat-explainer
  ([] (fn [_ _ pos coll k] (k pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs pos coll k]
       (r driver regs pos coll (fn [pos coll] (r* driver regs pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-explainer ?kr (reduce (fn [acc ?kr] (cat-explainer ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn cat-parser [& rs]
  (let [acc (reduce (fn [acc r]
                      (fn [driver regs coll* pos coll k]
                        (r driver regs pos coll
                           (fn [v pos coll] (acc driver regs (conj coll* v) pos coll k)))))
                    (fn [_ _ coll* pos coll k] (k coll* pos coll))
                    (reverse rs))]
    (fn [driver regs pos coll k] (acc driver regs [] pos coll k))))

(defn cat*-parser [& krs]
  (let [acc (reduce (fn [acc [tag r]]
                      (fn [driver regs m pos coll k]
                        (r driver regs pos coll
                           (fn [v pos coll] (acc driver regs (assoc m tag v) pos coll k)))))
                    (fn [_ _ m pos coll k] (k m pos coll))
                    (reverse krs))]
    (fn [driver regs pos coll k] (acc driver regs {} pos coll k))))

(defn cat-transformer
  ([] (fn [_ _ coll* pos coll k] (k coll* pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs coll* pos coll k]
       (r driver regs coll* pos coll (fn [coll* pos coll] (r* driver regs coll* pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-transformer ?kr (reduce (fn [acc ?kr] (cat-transformer ?kr acc)) (reverse (cons ?kr* ?krs))))))

;;;; ## Alternation

(defn alt-validator
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs pos coll k]
       (park-validator! driver r* regs pos coll k)          ; remember fallback
       (park-validator! driver r regs pos coll k))))
  ([?kr ?kr* & ?krs]
   (alt-validator ?kr (reduce (fn [acc ?kr] (alt-validator ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn alt-explainer
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs pos coll k]
       (park-explainer! driver r* regs pos coll k)          ; remember fallback
       (park-explainer! driver r regs pos coll k))))
  ([?kr ?kr* & ?krs]
   (alt-explainer ?kr (reduce (fn [acc ?kr] (alt-explainer ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn alt-parser
  ([r] r)
  ([r & rs]
   (reduce (fn [acc r]
             (fn [driver regs pos coll k]
               (park-validator! driver acc regs pos coll k) ; remember fallback
               (park-validator! driver r regs pos coll k)))
           (reverse (cons r rs)))))

(defn alt*-parser
  ([[tag r]] (fn [driver pos coll k] (r driver pos coll (fn [v pos coll] (k (-tagged tag v) pos coll)))))
  ([kr & krs]
   (let [krs (reverse (cons kr krs))]
     (reduce (fn [acc [tag r]]
               (fn [driver regs pos coll k]
                 (park-validator! driver acc regs pos coll k) ; remember fallback
                 (park-validator! driver r regs pos coll (fn [v pos coll] (k (-tagged tag v) pos coll)))))
             (alt*-parser (first krs))
             (rest krs)))))

(defn alt-transformer
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver regs coll* pos coll k]
       (park-transformer! driver r* regs coll* pos coll k)  ; remember fallback
       (park-transformer! driver r regs coll* pos coll k))))
  ([?kr ?kr* & ?krs]
   (alt-transformer ?kr (reduce (fn [acc ?kr] (alt-transformer ?kr acc)) (reverse (cons ?kr* ?krs))))))

;;;; ## Option

(defn ?-validator [p] (alt-validator p (cat-validator)))
(defn ?-explainer [p] (alt-explainer p (cat-explainer)))
(defn ?-parser [p] (alt-parser p (cat-parser)))
(defn ?-transformer [p] (alt-transformer p (cat-transformer)))

;;;; ## Kleene Star

(defn *-validator [p]
  (let [*p-epsilon (cat-validator)]
    (fn *p [driver regs pos coll k]
      (park-validator! driver *p-epsilon regs pos coll k)   ; remember fallback
      (p driver regs pos coll (fn [pos coll] (park-validator! driver *p regs pos coll k)))))) ; TCO

(defn *-explainer [p]
  (let [*p-epsilon (cat-explainer)]
    (fn *p [driver regs pos coll k]
      (park-explainer! driver *p-epsilon regs pos coll k)   ; remember fallback
      (p driver regs pos coll (fn [pos coll] (park-explainer! driver *p regs pos coll k)))))) ; TCO

(defn *-parser [p]
  (let [*p-epsilon (fn [_ _ coll* pos coll k] (k coll* pos coll))] ; TCO
    (fn *p
      ([driver regs pos coll k] (*p driver regs [] pos coll k))
      ([driver regs coll* pos coll k]
       (park-transformer! driver *p-epsilon regs coll* pos coll k) ; remember fallback
       (p driver regs pos coll
          (fn [v pos coll] (park-transformer! driver *p regs (conj coll* v) pos coll k))))))) ; TCO

(defn *-transformer [p]
  (let [*p-epsilon (cat-transformer)]
    (fn *p [driver regs coll* pos coll k]
      (park-transformer! driver *p-epsilon regs coll* pos coll k) ; remember fallback
      (p driver regs coll* pos coll
         (fn [coll* pos coll] (park-transformer! driver *p regs coll* pos coll k)))))) ; TCO

;;;; ## Non-Kleene Plus

(defn +-validator [p] (cat-validator p (*-validator p)))
(defn +-explainer [p] (cat-explainer p (*-explainer p)))
(defn +-parser [p] (cat-parser p (*-parser p)))
(defn +-transformer [p] (cat-transformer p (*-transformer p)))

;;;; ## Repeat

(defn repeat-validator [min max p]
  (let [rep-epsilon (cat-validator)]
    (letfn [(compulsories [driver regs pos coll k]
              (if (< (peek regs) min)
                (p driver regs pos coll
                   (fn [pos coll]
                     (-park-validator! driver
                                       (fn [driver stack pos coll k]
                                         (compulsories driver (conj (pop stack) (inc (peek stack))) pos coll k))
                                       regs pos coll k)))   ; TCO
                (optionals driver regs pos coll k)))
            (optionals [driver regs pos coll k]
              (if (< (peek regs) max)
                (do
                  (park-validator! driver rep-epsilon regs pos coll k) ; remember fallback
                  (p driver regs pos coll
                     (fn [pos coll]
                       (-park-validator! driver
                                         (fn [driver regs pos coll k]
                                           (optionals driver (conj (pop regs) (inc (peek regs))) pos coll k))
                                         regs pos coll k)))) ; TCO
                (k pos coll)))]
      (fn [driver regs pos coll k] (compulsories driver (conj regs 0) pos coll k)))))

(defn repeat-explainer [min max p]
  (let [rep-epsilon (cat-explainer)]
    (letfn [(compulsories [driver regs pos coll k]
              (if (< (peek regs) min)
                (p driver regs pos coll
                   (fn [pos coll]
                     (-park-explainer! driver
                                       (fn [driver regs pos coll k]
                                         (compulsories driver (conj (pop regs) (inc (peek regs))) pos coll k))
                                       regs pos coll k)))   ; TCO
                (optionals driver regs pos coll k)))
            (optionals [driver regs pos coll k]
              (if (< (peek regs) max)
                (do
                  (park-explainer! driver rep-epsilon regs pos coll k) ; remember fallback
                  (p driver regs pos coll
                     (fn [pos coll]
                       (-park-explainer! driver
                                         (fn [driver regs pos coll k]
                                           (optionals driver (conj (pop regs) (inc (peek regs))) pos coll k))
                                         regs pos coll k)))) ; TCO
                (k pos coll)))]
      (fn [driver regs pos coll k] (compulsories driver (conj regs 0) pos coll k)))))

(defn repeat-parser [min max p]
  (let [rep-epsilon (cat-parser)]
    (letfn [(compulsories [driver regs coll* pos coll k]
              (if (< (peek regs) min)
                (p driver regs pos coll
                   (fn [v pos coll]
                     (-park-transformer! driver
                                         (fn [driver regs coll* pos coll k]
                                           (compulsories driver (conj (pop regs) (inc (peek regs))) (conj coll* v) pos coll k))
                                         regs coll* pos coll k))) ; TCO
                (optionals driver regs coll* pos coll k)))
            (optionals [driver regs coll* pos coll k]
              (if (< (peek regs) max)
                (do
                  (park-transformer! driver rep-epsilon regs coll* pos coll k) ; remember fallback
                  (p driver regs pos coll
                     (fn [v pos coll]
                       (-park-transformer!
                         driver
                         (fn [driver regs coll* pos coll k]
                           (optionals driver (conj (pop regs) (inc (peek regs))) (conj coll* v) pos coll k))
                         regs coll* pos coll k))))          ; TCO
                (k pos coll)))]
      (fn [driver regs pos coll k] (compulsories driver (conj regs 0) [] pos coll k)))))

(defn repeat-transformer [min max p]
  (let [rep-epsilon (cat-transformer)]
    (letfn [(compulsories [driver regs coll* pos coll k]
              (if (< (peek regs) min)
                (p driver regs coll* pos coll
                   (fn [coll* pos coll]
                     (-park-transformer! driver
                                         (fn [driver regs coll* pos coll k]
                                           (compulsories driver (conj (pop regs) (inc (peek regs))) coll* pos coll k))
                                         regs coll* pos coll k))) ; TCO
                (optionals driver regs coll* pos coll k)))
            (optionals [driver regs coll* pos coll k]
              (if (< (peek regs) max)
                (do
                  (park-transformer! driver rep-epsilon regs coll* pos coll k) ; remember fallback
                  (p driver regs coll* pos coll
                     (fn [coll* pos coll]
                       (-park-transformer! driver
                                           (fn [driver regs coll* pos coll k]
                                             (optionals driver (conj (pop regs) (inc (peek regs))) coll* pos coll k))
                                           regs coll* pos coll k)))) ; TCO
                (k coll* pos coll)))]
      (fn [driver regs coll* pos coll k] (compulsories driver (conj regs 0) coll* pos coll k)))))

;;;; # Shared Drivers

(defn- make-stack [] #?(:clj (ArrayDeque.), :cljs #js []))

(defn- empty-stack? [^ArrayDeque stack] #?(:clj (.isEmpty stack), :cljs (zero? (alength stack))))

(defn- make-cache [] #?(:clj (HashSet.), :cljs (volatile! (transient #{}))))

(defn- ensure-cached! [^HashSet cache f pos regs]
  (let [entry [pos f regs]]
    #?(:clj  (or (.contains cache entry)
                 (do (.add cache entry) false))
       :cljs (or (contains? @cache entry)
                 (do (vswap! cache conj! entry) false)))))

(deftype ^:private CheckDriver
  #?(:clj  [^:unsynchronized-mutable ^boolean success, ^ArrayDeque stack, ^HashSet cache]
     :cljs [^:mutable success, stack, cache])

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (pop-thunk! [_] (when-not (empty-stack? stack) (.pop stack)))

  IValidationDriver
  (-park-validator! [self validator regs pos coll k] (.push stack #(validator self regs pos coll k)))
  (park-validator! [self validator regs pos coll k]
    (when-not (ensure-cached! cache validator pos regs)
      (-park-validator! self validator regs pos coll k))))

(deftype ^:private ParseDriver
  #?(:clj  [^:unsynchronized-mutable ^boolean success, ^ArrayDeque stack, cache
            ^:unsynchronized-mutable result]
     :cljs [^:mutable success, stack, cache, ^:mutable result])

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (pop-thunk! [_] (when-not (empty-stack? stack) (.pop stack)))

  IValidationDriver
  (-park-validator! [self validator regs pos coll k] (.push stack #(validator self regs pos coll k)))
  (park-validator! [self validator regs pos coll k]
    (when-not (ensure-cached! cache validator pos regs)
      (-park-validator! self validator regs pos coll k)))

  IParseDriver
  (-park-transformer! [driver transformer regs coll* pos coll k]
    (.push stack #(transformer driver regs coll* pos coll k)))
  (park-transformer! [driver transformer regs coll* pos coll k]
    (when-not (ensure-cached! cache transformer pos regs)
      (-park-transformer! driver transformer regs coll* pos coll k)))
  (succeed-with! [self v] (succeed! self) (set! result v))
  (success-result [_] result))

;;;; # Validator

(defn validator [p]
  (let [p (cat-validator p (end-validator))]
    (fn [coll]
      (and (sequential? coll)
           (let [driver (CheckDriver. false (make-stack) (make-cache))]
             (p driver () 0 coll (fn [_ _] (succeed! driver)))
             (or (succeeded? driver)
                 (loop []
                   (if-some [thunk (pop-thunk! driver)]
                     (do
                       (thunk)
                       (or (succeeded? driver) (recur)))
                     false))))))))

;;;; # Explainer

(deftype ^:private ExplanationDriver
  #?(:clj  [^:unsynchronized-mutable ^boolean success, ^ArrayDeque stack, cache
            in, ^:unsynchronized-mutable errors-max-pos, ^:unsynchronized-mutable errors]
     :cljs [^:mutable success, stack, cache, in, ^:mutable errors-max-pos, ^:mutable errors])

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (pop-thunk! [_] (when-not (empty-stack? stack) (.pop stack)))

  IExplanationDriver
  (-park-explainer! [self validator regs pos coll k] (.push stack #(validator self regs pos coll k)))
  (park-explainer! [self validator regs pos coll k]
    (when-not (ensure-cached! cache validator pos regs)
      (-park-explainer! self validator regs pos coll k)))
  (value-path [_ pos] (conj in pos))
  (fail! [_ pos errors*]
    (cond
      (> pos errors-max-pos) (do
                               (set! errors-max-pos pos)
                               (set! errors errors*))
      (= pos errors-max-pos) (set! errors (into errors errors*))))
  (latest-errors [_] errors))

(defn explainer [schema path p]
  (let [p (cat-explainer p (end-explainer schema path))]
    (fn [coll in errors]
      (if (sequential? coll)
        (let [pos 0
              driver (ExplanationDriver. false (make-stack) (make-cache) in pos [])]
          (p driver () pos coll (fn [_ _] (succeed! driver)))
          (if (succeeded? driver)
            errors
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) errors (recur)))
                (into errors (latest-errors driver))))))
        (conj errors (-error path in schema coll :malli.core/invalid-type))))))

;;;; # Parser

(defn parser [p]
  (let [p (cat-parser p (end-parser))]
    (fn [coll]
      (if (sequential? coll)
        (let [driver (ParseDriver. false (make-stack) (make-cache) nil)]
          (p driver () 0 coll (fn [v _ _] (succeed-with! driver v)))
          (if (succeeded? driver)
            (first (success-result driver))
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) (first (success-result driver)) (recur)))
                (-fail! :malli.core/nonconforming)))))
        (-fail! :malli.core/nonconforming)))))

;;;; # Transformer

(defn transformer [p]
  (let [p (cat-transformer p (end-transformer))]
    (fn [coll]
      (if (sequential? coll)
        (let [driver (ParseDriver. false (make-stack) (make-cache) nil)]
          (p driver () [] 0 coll (fn [coll* _ _] (succeed-with! driver coll*)))
          (if (succeeded? driver)
            (success-result driver)
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) (success-result driver) (recur)))
                coll))))
        coll))))
