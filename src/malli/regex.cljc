(ns malli.regex
  (:refer-clojure :exclude [+ * repeat cat compile])
  #?(:cljs (:require-macros [malli.regex.macros :refer [asm opcode-case]]))
  (:require [malli.regex.compiler :refer [compile]]
            #?(:clj [malli.regex.macros :refer [asm opcode-case]]))
  #?(:clj (:import [malli.regex.compiler CompiledPattern]
                   [clojure.lang MapEntry PersistentList APersistentVector]
                   [java.util Arrays])))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))

;;;; Combinators

(def ^:private anonymous (object-array 0))

(defprotocol ^:private MatchFrame
  (-add-match [self k v])
  (-children [self k buf]))

(deftype ^:private StartFrame [child, ^long start]
  MatchFrame
  (-add-match [_ _ v] (StartFrame. v start))
  (-children [_ _ _] child))

(defn- start-frame [^long start] (StartFrame. anonymous start))

;; OPTIMIZE: PredFrames should be unnecessary:
(deftype ^:private PredFrame [^long start]
  MatchFrame
  (-add-match [_ _ _] (throw (ex-info "unreachable" {})))
  (-children [_ _ buf] (peek buf)))

(def ^:private pred-frame ->PredFrame)

(defn is [pred]
  (asm
    save0 pred-frame
    pred pred
    save1 anonymous))

(deftype ^:private Eczema [explain schema])

(defn explain-item [schema explainer] (asm explain (Eczema. explainer schema)))

(deftype ^:private GuardedTransformer [validate transformer])

(defn item-transformer [validator transformer]
  (asm encoder (GuardedTransformer. validator transformer)))

(defn decoded-item [transformer validator]
  (asm decode (GuardedTransformer. validator transformer)))

(deftype ^:private CatFrame [children, ^long start]
  MatchFrame
  (-add-match [_ k v] (CatFrame. (assoc children k v) start))
  (-children [_ _ _] children))

(defn- cat-frame [^long start] (CatFrame. {} start))

(defn cat [& rs]
  (asm
    save0 cat-frame
    include (mapcat (fn [?kr]
                      (if (vector? ?kr)
                        (asm
                          save0 start-frame
                          include (second ?kr)
                          save1 (first ?kr))
                        ?kr))
                    rs)
    save1 anonymous))

(deftype ^:private AltFrame [val, ^long start]
  MatchFrame
  (-add-match [_ _ v] (AltFrame. v start))
  (-children [_ k _] (-tagged k val)))

(defn- alt-frame [^long start] (AltFrame. nil start))

(defn alt
  ([] (asm pred (fn [_] false)))
  ([?kr]
   (if (vector? ?kr)
     (asm
       save0 alt-frame
       include (second ?kr)
       save1 (first ?kr))
     ?kr))
  ([?kr & ?krs]
   (asm
     fork> l1
     include (alt ?kr)
     jump l2
     label l1
     include (apply alt ?krs)
     label l2)))

(deftype ^:private OptFrame [?child, ^long start]
  MatchFrame
  (-add-match [_ _ v] (OptFrame. v start))
  (-children [_ _ _] ?child))

(defn- opt-frame [start] (OptFrame. nil start))

(defn ? [r]
  (asm
    save0 opt-frame
    fork> end
    include r
    label end
    save1 anonymous))

(deftype ^:private ManyFrame [children, ^long start]
  MatchFrame
  (-add-match [_ _ v] (ManyFrame. (conj children v) start))
  (-children [_ _ _] children))

(defn- many-frame [start] (ManyFrame. [] start))

(defn * [r]
  (asm
    save0 many-frame
    label start
    fork> end
    include r
    jump start
    label end
    save1 anonymous))

(defn + [r]
  (asm
    save0 many-frame
    label start
    include r
    fork< start
    save1 anonymous))

(defn repeat [min max r]
  (cond
    (pos? min) (cat (apply cat (clojure.core/repeat min r)) (repeat 0 (- max min) r))
    (pos? max) (? (cat r (repeat 0 (dec max) r)))
    :else (asm)))

;;;; Pike VM

(def ^:private ^:const +int-bitwidth+ 32)

(defn- make-bitset ^ints [max-val]
  (int-array (quot (dec (clojure.core/+ max-val +int-bitwidth+)) +int-bitwidth+)))

(defn- clear-bitset! [^ints set]
  #?(:clj  (Arrays/fill set (byte 0))
     :cljs (.fill set 0)))

(defn- bitset-set! [^ints set, ^long v]
  (let [coarse-index (quot v +int-bitwidth+)
        bit-index (rem v +int-bitwidth+)]
    (aset set coarse-index (unchecked-int (bit-or (aget set coarse-index)
                                                  (bit-shift-left 1 bit-index))))
    nil))

(defn- bitset-contains? [^ints set, ^long v]
  (let [coarse-index (quot v +int-bitwidth+)
        bit-index (rem v +int-bitwidth+)]
    (= (bit-and (bit-shift-right (aget set coarse-index) bit-index)
                1)
       1)))

(defprotocol RegisterBank
  (save0 [bank id start])
  (save1 [bank id buf end])
  (fetch [bank buf coll]))

(deftype ^:private SinkBank [res]
  RegisterBank
  (save0 [self _ _] self)
  (save1 [self _ _ _] self)
  (fetch [_ _ _] res))

(def ^:private sink-bank-succ (SinkBank. true))
(def ^:private sink-bank-fail (SinkBank. nil))

(extend-protocol RegisterBank
  #?(:clj PersistentList, :cljs List)
  (save0 [stack ->frame start] (conj stack (->frame start)))
  (save1 [stack k buf _]
    ;; 'return' `node`:
    (let [callee-frame (peek stack)
          node (-children callee-frame k buf)
          stack (pop stack)]
      ;; Update 'caller' state:
      (conj (pop stack) (-add-match (peek stack) k node))))
  (fetch [stack buf _] (-children (peek stack) nil buf))

  APersistentVector
  (save0 [self _ _] self)
  (save1 [self _ _ _] self)
  (fetch [self _ _] self)

  MapEntry
  (save0 [self _ _] self)
  (save1 [self _ _ _] self)
  (fetch [self _ _] self))

(defprotocol ^:private IVMState
  (thread-count [state])
  (fork! [state pc matches])
  (truncate! [state]))

(deftype ^:private VMState [#?(:clj ^:unsynchronized-mutable ^long nthreads, :cljs ^:mutable nthreads)
                            pcs matchess]
  IVMState
  (thread-count [_] nthreads)

  (fork! [_ pc matches]
    (let [i nthreads]
      (set! nthreads (inc i))
      (aset ^longs pcs i ^long pc)
      (aset ^"[Ljava.lang.Object;" matchess i matches)))

  (truncate! [_] (set! nthreads 0)))

(#?(:clj definterface, :cljs defprotocol) ^:private VM
  (clear_visited [#?@(:cljs [vm])])
  (match_item [#?@(:cljs [vm]) ^long pos, coll, buf, ^malli.regex.VMState state, ^malli.regex.VMState state*])
  (add_thread [#?@(:cljs [vm]) ^long pos, buf, ^malli.regex.VMState state, ^long pc, matches])
  (final_errors [#?@(:cljs [vm])]))

(defn- add-thread! [^VM vm pos buf state pc matches]
  (#?(:clj .clear_visited, :cljs clear_visited) vm)
  (#?(:clj .add_thread, :cljs add_thread) vm pos buf state pc matches))

(deftype ^:private PikeVM [^bytes opcodes, ^"[Ljava.lang.Object;" args, visited]
  VM
  (clear_visited [_] (clear-bitset! visited))

  (add_thread [self pos buf state pc matches]
    (cond
      (< pc (alength opcodes))
      (when-not (bitset-contains? visited pc)
        (bitset-set! visited pc)
        (opcode-case (aget opcodes pc)
          jump (recur pos buf state (clojure.core/+ pc (long (aget args pc))) matches)
          fork> (do
                  (#?(:clj .add_thread, :cljs add_thread) self pos buf state (inc pc) matches)
                  (recur pos buf state (clojure.core/+ pc (long (aget args pc))) matches))
          fork< (do
                  (#?(:clj .add_thread, :cljs add_thread) self pos buf state (clojure.core/+ pc (long (aget args pc))) matches)
                  (recur pos buf state (inc pc) matches))

          save0 (recur pos buf state (inc pc) (save0 matches (aget args pc) pos))
          save1 (recur pos buf state (inc pc) (save1 matches (aget args pc) buf pos))

          (pred explain encoder decode end) (fork! state pc matches)))

      (= pc (alength opcodes)) (fork! state pc matches)

      :else nil))

  (match_item [self pos coll buf state state*]
    (loop [i 0]
      (when (< i (thread-count state))
        (let [pc (aget ^longs (.-pcs state) i)
              matches (aget ^"[Ljava.lang.Object;" (.-matchess state) i)]
          (if (< pc (alength opcodes))
            (let [opcode (aget opcodes pc)
                  arg (aget args pc)]
              (opcode-case opcode
                pred (do
                       (when (and coll (arg (first coll)))
                         (add-thread! self (inc pos) (conj buf (first coll)) state* (inc pc) matches))
                       (recur (inc i)))
                encoder (let [^GuardedTransformer arg arg]
                          (when (and coll ((.-validate arg) (first coll)))
                            (add-thread! self (inc pos) (conj buf (first coll)) state* (inc pc)
                                         (conj matches (.-transformer arg))))
                          (recur (inc i)))
                decode (let [^GuardedTransformer arg arg]
                         (when coll
                           (let [v (if-some [enter (:enter (.-transformer arg))]
                                     (enter (first coll))
                                     (first coll))]
                             (when ((.-validate arg) v)
                               (add-thread! self (inc pos) (conj buf (first coll)) state* (inc pc)
                                            (-tagged (conj (key matches) v)
                                                     (conj (val matches) (.-transformer arg)))))))
                         (recur (inc i)))
                end (if coll
                      (recur (inc i))
                      matches)
                #_"add-thread! makes other opcodes impossible at this point"))
            matches)))))

  (final_errors [_] nil))

(deftype ^:private ExplanatoryVM [^PikeVM super, path, in, -error
                                  #?(:clj ^:unsynchronized-mutable errors, :cljs ^:mutable errors)]
  VM
  (clear_visited [_] (#?(:clj .clear_visited, :cljs clear_visited) super))
  (add_thread [_ pos buf state pc matches]
    (#?(:clj .add_thread, :cljs add_thread) super pos buf state pc matches))

  (match_item [self pos coll buf state state*]
    (loop [i 0, errors* []]
      (if (< i (thread-count state))
        (let [^bytes opcodes (.-opcodes super)
              pc (aget ^longs (.-pcs state) i)
              matches (aget ^"[Ljava.lang.Object;" (.-matchess state) i)]
          (if (< pc (alength opcodes))
            (let [opcode (aget opcodes pc)
                  arg (aget ^"[Ljava.lang.Object;" (.-args super) pc)
                  in (conj in pos)]
              (opcode-case opcode
                explain (if coll
                          (let [errors** ((.-explain ^Eczema arg) (first coll) in errors*)]
                            (when (identical? errors** errors*)
                              (add-thread! self (inc pos) (conj buf (first coll)) state* (inc pc) matches))
                            (recur (inc i) errors**))
                          (recur (inc i) (conj errors* (-error path in (.-schema ^Eczema arg) nil ::end-of-input))))
                end (if coll
                      (recur (inc i) (conj errors* (-error path in arg coll ::input-remaining)))
                      matches)
                #_"add-thread! makes other opcodes impossible at this point"))
            matches))
        (do (set! errors errors*) nil))))

  (final_errors [_] errors))

(defn- ->vm ^PikeVM [^CompiledPattern automaton]
  (let [^bytes opcodes (.-opcodes automaton)]
    (PikeVM. opcodes (.-args automaton) (make-bitset (alength opcodes)))))

(defn- ->explanatory-vm ^ExplanatoryVM [automaton path in -error]
  (ExplanatoryVM. (->vm automaton) path in -error []))

(defn- exec-automaton* [^CompiledPattern automaton, ^VM vm, coll0, registers]
  (let [^bytes opcodes (.-opcodes automaton)
        inst-count (alength opcodes)
        state (VMState. 0 (long-array inst-count) (object-array inst-count))
        state* (VMState. 0 (long-array inst-count) (object-array inst-count))]
    (add-thread! vm 0 () state 0 registers)
    (loop [pos 0, coll (seq coll0), buf (), state state, state* state*, longest nil]
      (let [longest (or (#?(:clj .match_item, :cljs match_item) vm pos coll buf state state*) longest)]
        (if (and (> (thread-count state*) 0) coll)
          (do
            (truncate! state)
            (recur (inc pos) (next coll) (conj buf (first coll)) state* state longest))
          (if longest
            (fetch longest buf (or coll ()))
            (#?(:clj .final_errors, :cljs final_errors) vm)))))))

(defn exec-recognizer [automaton coll]
  (boolean (exec-automaton* automaton (->vm automaton) coll sink-bank-succ)))

(defn exec-explainer [automaton path coll in -error]
  (exec-automaton* automaton (->explanatory-vm automaton path in -error) coll sink-bank-fail))

(defn exec-encoder-assignment [automaton coll]
  (exec-automaton* automaton (->vm automaton) coll []))

(defn exec-decoder-assignment [automaton coll]
  (exec-automaton* automaton (->vm automaton) coll (-tagged [] [])))

(defn exec-tree-automaton [automaton coll]
  (exec-automaton* automaton (->vm automaton) coll (list (start-frame 0))))

;;;; Malli APIs

(defn parser [re]
  (let [automaton (compile re)]
    (fn [x]
      (and (sequential? x)
           (exec-tree-automaton automaton x)))))

;;; TODO: Non-regex and nested destructuring

(defn parse [re coll] ((parser re) coll))

;;;; Backtracker

(defprotocol ^:private Driver
  (succeed! [self])
  (succeeded? [self])
  (pop-thunk! [self]))

(defprotocol ^:private IValidationDriver
  (-park-validator! [driver validator pos coll k])
  (park-validator! [driver validator pos coll k]))

(defprotocol ^:private IExplanationDriver
  (-park-explainer! [driver explainer pos coll k])
  (park-explainer! [driver explainer pos coll k])
  (value-path [self pos])
  (fail! [self pos errors*])
  (latest-errors [self]))

(defprotocol ^:private IParseDriver
  (-park-transformer! [driver transformer coll* pos coll k])
  (park-transformer! [driver transformer coll* pos coll k])
  (succeed-with! [self v])
  (success-result [self]))

;;;; # Primitives

;;;; ## Seq Item

(defn item-validator [valid?]
  (fn [_ pos coll k]
    (when (and (seq coll) (valid? (first coll)))
      (k (inc pos) (rest coll)))))

(defn item-explainer [-error path schema schema-explainer]
  (fn [driver pos coll k]
    (let [in (value-path driver pos)]
      (if (seq coll)
        (let [errors (schema-explainer (first coll) in [])]
          (if (seq errors)
            (fail! driver pos errors)
            (k (inc pos) (rest coll))))
        (fail! driver pos [(-error path in schema nil ::end-of-input)])))))

(defn item-parser [valid?]
  (fn [_ pos coll k]
    (when (seq coll)
      (let [v (first coll)]
        (when (valid? v)
          (k v (inc pos) (rest coll)))))))

(defn item-encoder [valid? encode]
  (fn [_ coll* pos coll k]
    (when (seq coll)
      (let [v (first coll)]
        (when (valid? v)
          (k (conj coll* (encode v)) (inc pos) (rest coll)))))))

(defn item-decoder [decode valid?]
  (fn [_ coll* pos coll k]
    (when (seq coll)
      (let [v (decode (first coll))]
        (when (valid? v)
          (k (conj coll* v) (inc pos) (rest coll)))))))

;;;; ## End of Seq

(defn end-validator [] (fn [_ pos coll k] (when (empty? coll) (k pos coll))))

(defn end-explainer [-error schema path]
  (fn [driver pos coll k]
    (if (empty? coll)
      (k pos coll)
      (fail! driver pos (list (-error path (value-path driver pos) schema (first coll) ::input-remaining))))))

(defn end-parser [] (fn [_ pos coll k] (when (empty? coll) (k nil pos coll))))

(defn end-transformer [] (fn [_ coll* pos coll k] (when (empty? coll) (k coll* pos coll))))

;;;; # Combinators

;;;; ## Catenation

(defn- entry->regex [?kr] (if (vector? ?kr) (get ?kr 1) ?kr))

(defn cat-validator
  ([] (fn [_ pos coll k] (k pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver pos coll k]
       (r driver pos coll (fn [pos coll] (r* driver pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-validator ?kr (reduce (fn [acc ?kr] (cat-validator ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn cat-explainer
  ([] (fn [_ pos coll k] (k pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver pos coll k]
       (r driver pos coll (fn [pos coll] (r* driver pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-explainer ?kr (reduce (fn [acc ?kr] (cat-explainer ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn- keyed-entry [i ?kr] (if (vector? ?kr) ?kr [i ?kr]))

(defn cat-parser
  ([] (fn [_ pos coll k] (k {} pos coll)))
  ([& ?krs]
   (transduce (map-indexed vector)
              (completing
                (fn [r [i ?kr]]
                  (let [[tag* r*] (keyed-entry i ?kr)]
                    (fn [driver pos coll k]
                      (r driver pos coll
                         (fn [m pos coll] (r* driver pos coll
                                              (fn [v* pos coll] (k (assoc m tag* v*) pos coll)))))))))
              (cat-parser) ?krs)))

(defn cat-transformer
  ([] (fn [_ coll* pos coll k] (k coll* pos coll)))
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver coll* pos coll k]
       (r driver coll* pos coll (fn [coll* pos coll] (r* driver coll* pos coll k))))))
  ([?kr ?kr* & ?krs]
   (cat-transformer ?kr (reduce (fn [acc ?kr] (cat-transformer ?kr acc)) (reverse (cons ?kr* ?krs))))))

;;;; ## Alternation

(defn alt-validator
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver pos coll k]
       (park-validator! driver r* pos coll k)               ; remember fallback
       (park-validator! driver r pos coll k))))
  ([?kr ?kr* & ?krs]
   (alt-validator ?kr (reduce (fn [acc ?kr] (alt-validator ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn alt-explainer
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver pos coll k]
       (park-explainer! driver r* pos coll k)               ; remember fallback
       (park-explainer! driver r pos coll k))))
  ([?kr ?kr* & ?krs]
   (alt-explainer ?kr (reduce (fn [acc ?kr] (alt-explainer ?kr acc)) (reverse (cons ?kr* ?krs))))))

(defn alt-parser
  ([?kr]
   (let [[tag r] (keyed-entry 0 ?kr)]
     (fn [driver pos coll k]
       (r driver pos coll (fn [v pos coll] (k (-tagged tag v) pos coll))))))
  ([?kr & ?krs]
   (transduce (map-indexed vector)
              (completing
                (fn [r [i ?kr]]
                  (let [[tag* r*] (keyed-entry i ?kr)]
                    (fn [driver pos coll k]
                      (park-validator! driver r* pos coll   ; remember fallback
                                       (fn [v* pos coll] (k (-tagged tag* v*) pos coll)))
                      (park-validator! driver r pos coll k)))))
              (alt-parser ?kr) ?krs)))

(defn alt-transformer
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver coll* pos coll k]
       (park-transformer! driver r* coll* pos coll k)       ; remember fallback
       (park-transformer! driver r coll* pos coll k))))
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
    (fn *p [driver pos coll k]
      (park-validator! driver *p-epsilon pos coll k)        ; remember fallback
      (p driver pos coll (fn [pos coll] (park-validator! driver *p pos coll k)))))) ; TCO

(defn *-explainer [p]
  (let [*p-epsilon (cat-explainer)]
    (fn *p [driver pos coll k]
      (park-explainer! driver *p-epsilon pos coll k)        ; remember fallback
      (p driver pos coll (fn [pos coll] (park-explainer! driver *p pos coll k)))))) ; TCO

(defn *-parser [p]
  (let [*p-epsilon (fn [_ coll* pos coll k] (k coll* pos coll))] ; TCO
    (fn *p
      ([driver pos coll k] (*p driver [] pos coll k))
      ([driver coll* pos coll k]
       (park-transformer! driver *p-epsilon coll* pos coll k) ; remember fallback
       (p driver pos coll (fn [v pos coll] (park-transformer! driver (conj coll* v) *p pos coll k)))))))

(defn *-transformer [p]
  (let [*p-epsilon (cat-transformer)]
    (fn *p [driver coll* pos coll k]
      (park-transformer! driver *p-epsilon coll* pos coll k) ; remember fallback
      (p driver coll* pos coll (fn [coll* pos coll] (park-transformer! driver *p coll* pos coll k)))))) ; TCO

;;;; ## Non-Kleene Plus

(defn +-validator [p] (cat-validator p (*-validator p)))
(defn +-explainer [p] (cat-explainer p (*-explainer p)))
(defn +-parser [p] (cat-parser p (*-parser p)))
(defn +-transformer [p] (cat-transformer p (*-transformer p)))

;;;; ## Repeat

(defn repeat-validator [min max p]
  (let [rep-epsilon (cat-validator)]
    (letfn [(compulsories [driver n pos coll k]
              (if (< n min)
                (p driver pos coll
                   (fn [pos coll]
                     (-park-validator! driver
                                       (fn [driver pos coll k] (compulsories driver (inc n) pos coll k))
                                       pos coll k)))        ; TCO
                (optionals driver n pos coll k)))
            (optionals [driver n pos coll k]
              (if (< n max)
                (do
                  (park-validator! driver rep-epsilon pos coll k) ; remember fallback
                  (p driver pos coll
                     (fn [pos coll]
                       (-park-validator! driver
                                         (fn [driver pos coll k] (optionals driver (inc n) pos coll k))
                                         pos coll k))))     ; TCO
                (k pos coll)))]
      (fn [driver pos coll k] (compulsories driver 0 pos coll k)))))

(defn repeat-explainer [min max p]
  (let [rep-epsilon (cat-explainer)]
    (letfn [(compulsories [driver n pos coll k]
              (if (< n min)
                (p driver pos coll
                   (fn [pos coll]
                     (-park-explainer! driver
                                       (fn [driver pos coll k] (compulsories driver (inc n) pos coll k))
                                       pos coll k)))        ; TCO
                (optionals driver n pos coll k)))
            (optionals [driver n pos coll k]
              (if (< n max)
                (do
                  (park-explainer! driver rep-epsilon pos coll k) ; remember fallback
                  (p driver pos coll
                     (fn [pos coll]
                       (-park-explainer! driver
                                         (fn [driver pos coll k] (optionals driver (inc n) pos coll k))
                                         pos coll k))))     ; TCO
                (k pos coll)))]
      (fn [driver pos coll k] (compulsories driver 0 pos coll k)))))

(defn repeat-parser [min max p]
  (let [rep-epsilon (cat-parser)]
    (letfn [(compulsories [driver n coll* pos coll k]
              (if (< n min)
                (p driver pos coll
                   (fn [v pos coll]
                     (-park-transformer!
                       driver
                       (fn [driver coll* pos coll k] (compulsories driver (inc n) (conj coll* v) pos coll k))
                       coll* pos coll k)))                  ; TCO
                (optionals driver n coll* pos coll k)))
            (optionals [driver n coll* pos coll k]
              (if (< n max)
                (do
                  (park-transformer! driver rep-epsilon coll* pos coll k) ; remember fallback
                  (p driver pos coll
                     (fn [v pos coll]
                       (-park-transformer!
                         driver
                         (fn [driver coll* pos coll k] (optionals driver (inc n) (conj coll* v) pos coll k))
                         coll* pos coll k))))               ; TCO
                (k pos coll)))]
      (fn [driver pos coll k] (compulsories driver 0 [] pos coll k)))))

(defn repeat-transformer [min max p]
  (let [rep-epsilon (cat-transformer)]
    (letfn [(compulsories [driver n coll* pos coll k]
              (if (< n min)
                (p driver coll* pos coll
                   (fn [coll* pos coll]
                     (-park-transformer! driver
                                         (fn [driver coll* pos coll k] (compulsories driver (inc n) coll* pos coll k))
                                         coll* pos coll k))) ; TCO
                (optionals driver n coll* pos coll k)))
            (optionals [driver n coll* pos coll k]
              (if (< n max)
                (do
                  (park-transformer! driver rep-epsilon coll* pos coll k) ; remember fallback
                  (p driver pos coll
                     (fn [coll* pos coll]
                       (-park-transformer! driver
                                           (fn [driver coll* pos coll k] (optionals driver (inc n) coll* pos coll k))
                                           coll* pos coll k)))) ; TCO
                (k coll* pos coll)))]
      (fn [driver coll* pos coll k] (compulsories driver 0 coll* pos coll k)))))

;;;; # Shared Drivers

(deftype ^:private CheckDriver
  [^:unsynchronized-mutable ^boolean success, ^java.util.ArrayDeque stack, ^java.util.IdentityHashMap cache]

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (pop-thunk! [_] (when-not (.isEmpty stack) (.pop stack)))

  IValidationDriver
  (-park-validator! [self validator pos coll k] (.push stack #(validator self pos coll k)))
  (park-validator! [self validator pos coll k]
    (if-some [^java.util.HashSet pos-cache (.get cache validator)]
      (when-not (.contains pos-cache pos)
        (.add pos-cache pos)
        (-park-validator! self validator pos coll k))
      (let [pos-cache (java.util.HashSet.)]
        (.put cache validator pos-cache)
        (.add pos-cache pos)
        (-park-validator! self validator pos coll k)))))

(deftype ^:private ParseDriver
  [^:unsynchronized-mutable ^boolean success, ^java.util.ArrayDeque stack, ^java.util.IdentityHashMap cache
   ^:unsynchronized-mutable result]

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (pop-thunk! [_] (when-not (.isEmpty stack) (.pop stack)))

  IValidationDriver
  (-park-validator! [self validator pos coll k] (.push stack #(validator self pos coll k)))
  (park-validator! [self validator pos coll k]
    (if-some [^java.util.HashSet pos-cache (.get cache validator)]
      (when-not (.contains pos-cache pos)
        (.add pos-cache pos)
        (-park-validator! self validator pos coll k))
      (let [pos-cache (java.util.HashSet.)]
        (.put cache validator pos-cache)
        (.add pos-cache pos)
        (-park-validator! self validator pos coll k))))

  IParseDriver
  (-park-transformer! [driver transformer coll* pos coll k] (.push stack #(transformer driver coll* pos coll k)))
  (park-transformer! [driver transformer coll* pos coll k]
    (if-some [^java.util.HashSet pos-cache (.get cache transformer)]
      (when-not (.contains pos-cache pos)
        (.add pos-cache pos)
        (-park-transformer! driver transformer coll* pos coll k))
      (let [pos-cache (java.util.HashSet.)]
        (.put cache transformer pos-cache)
        (.add pos-cache pos)
        (-park-transformer! driver transformer coll* pos coll k))))
  (succeed-with! [self v] (succeed! self) (set! result v))
  (success-result [_] result))

;;;; # Validator

(defn validator [p]
  (let [p (cat-validator p (end-validator))]
    (fn [coll]
      (and (sequential? coll)
           (let [driver (CheckDriver. false (java.util.ArrayDeque.) (java.util.IdentityHashMap.))]
             (p driver 0 coll (fn [_ _] (succeed! driver)))
             (or (succeeded? driver)
                 (loop []
                   (if-some [thunk (pop-thunk! driver)]
                     (do
                       (thunk)
                       (or (succeeded? driver) (recur)))
                     false))))))))

;;;; # Explainer

(deftype ^:private ExplanationDriver
  [check-driver, in, ^:unsynchronized-mutable errors-max-pos, ^:unsynchronized-mutable errors]

  Driver
  (succeed! [_] (succeed! check-driver))
  (succeeded? [_] (succeeded? check-driver))
  (pop-thunk! [_] (pop-thunk! check-driver))

  IExplanationDriver
  (-park-explainer! [_ explainer pos coll k] (-park-validator! check-driver explainer pos coll k))
  (park-explainer! [_ explainer pos coll k] (park-validator! check-driver explainer pos coll k))
  (value-path [_ pos] (conj in pos))
  (fail! [_ pos errors*]
    (cond
      (> pos errors-max-pos) (do
                               (set! errors-max-pos pos)
                               (set! errors errors*))
      (= pos errors-max-pos) (set! errors (into errors errors*))))
  (latest-errors [_] errors))

(defn explainer [-error schema path p]
  (let [p (cat-explainer p (end-explainer -error schema path))]
    (fn [coll in errors]
      (if (sequential? coll)
        (let [pos 0
              driver (ExplanationDriver. (CheckDriver. false (java.util.ArrayDeque.) (java.util.IdentityHashMap.))
                                         in pos [])]
          (p driver pos coll (fn [_ _] (succeed! driver)))
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

(defn parser [-error path schema p]
  (let [p (cat-parser p (end-parser))]
    (fn [coll in]
      (if (sequential? coll)
        (let [driver (ParseDriver. false (java.util.ArrayDeque.) (java.util.IdentityHashMap.) nil)]
          (p driver 0 coll (fn [v _ _] (succeed-with! driver v)))
          (if (succeeded? driver)
            (success-result driver)
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) (success-result driver) (recur)))
                (-error path in schema coll)))))
        (-error path in schema coll)))))

;;;; # Transformer

(defn transformer [p]
  (let [p (cat-transformer p (end-transformer))]
    (fn [coll]
      (if (sequential? coll)
        (let [driver (ParseDriver. false (java.util.ArrayDeque.) (java.util.IdentityHashMap.) nil)]
          (p driver [] 0 coll (fn [coll* _ _] (succeed-with! driver coll*)))
          (if (succeeded? driver)
            (success-result driver)
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) (success-result driver) (recur)))
                coll))))
        coll))))
