(ns malli.regex
  (:refer-clojure :exclude #_[fn cat repeat ? * +] [+ * repeat cat compile])
  #?(:cljs (:require-macros [malli.regex.macros :refer [asm opcode-case]]))
  (:require [malli.regex.compiler :refer [compile]]
            #?(:clj [malli.regex.macros :refer [asm opcode-case]]))
  #?(:clj (:import [malli.regex.compiler CompiledPattern]
                   [clojure.lang MapEntry PersistentList APersistentVector]
                   [java.util Arrays])))

#_(
   (defprotocol RegexSchema
     (-matcher [_]))

   (defprotocol Match
     (-end [_])
     (-result [_])
     (-continue [_]))

   (defn -tagged [k v]
     #?(:clj (clojure.lang.MapEntry. k v), :cljs (MapEntry. tag ret nil)))

   (defn -parse [rs]
     (reduce
       (clojure.core/fn [[ms im] [i ?kr]]
         (let [[k r] (if (vector? ?kr) [(first ?kr) (second ?kr)] [nil ?kr])]
           [(conj ms (-matcher r)) (cond-> im k (assoc i k))]))
       [[]] (->> rs (vec) (map-indexed vector))))

   (defn -static [result end]
     #_(prn "-static" result end)
     (reify Match
       (-end [_] end)
       (-result [_] result)
       (-continue [_])))

   (defn fn [f]
     (reify RegexSchema
       (-matcher [_]
         (clojure.core/fn [data size start]
           #_(prn "FN:" data start f)
           (if (< -1 start size)
             (let [x (nth data start)]
               (if (f x) (-static x start))))))))

   (defn lazy [f]
     (reify RegexSchema
       (-matcher [_]
         (clojure.core/fn [data size start]
           ((-matcher (f)) data size start)))))

   (defn alt [& rs]
     (let [[matchers i->k] (-parse rs)
           matchers-size (count matchers)
           -key (or i->k identity)
           -value (clojure.core/fn -value [res i f]
                    (reify Match
                      (-end [_] (-end res))
                      (-result [_] (-tagged (-key i) (-result res)))
                      (-continue [_] (f (inc i)))))
           -continue (clojure.core/fn -continue [data size start i]
                       (loop [i i]
                         (if-not (= i matchers-size)
                           (let [m (nth matchers i)]
                             (or (some-> (m data size start) (-value i (partial -continue data size start)))
                                 (recur (inc i)))))))]
       (reify RegexSchema
         (-matcher [_] (clojure.core/fn [data size start] (-continue data size start 0))))))

   (defn cat [& rs]
     (let [[matchers i->k] (-parse rs)
           -result (if i->k #(->> % (map (clojure.core/fn [[k v]] [(i->k k) (-result v)])) (into {}))
                            #(->> % (mapv (comp -result val))))
           matchers-size (count matchers)
           -value (clojure.core/fn -value [m i]
                    (reify Match
                      (-end [_] (-end (m i)))
                      (-result [_] (-result m))
                      (-continue [_]
                        (loop [si i]
                          (when-not (neg? si)
                            (if-let [sr (some-> si m -continue)]
                              (-value (assoc m si sr) i)
                              (recur (dec si))))))))]
       (reify RegexSchema
         (-matcher [_]
           (clojure.core/fn [data size start]
             (loop [i 0, splits [start], results {}]
               #_(prn "CAT:" (vals i->k) i splits results)
               (cond
                 (neg? i) nil
                 (results i) (if-let [res (-continue (results i))]
                               (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                               (recur (dec i) (pop splits) (dissoc results i)))
                 (>= i matchers-size) (-value results (dec i))
                 :else (do
                         #_(prn "looking..." ((nth matchers i) data size (nth splits i)))
                         (if-let [res ((nth matchers i) data size (nth splits i))]
                           (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                           (if (pos? i) (recur (dec i) (pop splits) (dissoc results i))))))))))))

   (defn repeat [min max r]
     (let [child-matcher (-matcher r)
           -value (clojure.core/fn -value [v i]
                    (reify Match
                      (-end [_] (-end (v i)))
                      (-result [_] (mapv -result v))
                      (-continue [this]
                        (loop [si i]
                          (when-not (neg? si)
                            (let [sr (some-> si v -continue)]
                              (cond
                                sr (-value (assoc v si sr) (dec i))
                                (= i min 0) (-static [] (dec (-end this)))
                                (>= i min) (-value (pop v) (dec i))
                                :else (recur (dec si)))))))))]
       (reify RegexSchema
         (-matcher [_]
           (clojure.core/fn [data size start]
             (loop [i 0, splits [start], results []]
               #_(prn "REP:" min max i data splits results)
               (cond
                 (contains? results i) (if-let [res (-continue (results i))]
                                         (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                                         (recur (dec i) (pop splits) (pop results)))
                 (>= i max) (-value results (dec i))
                 (and (>= i min) (= (last splits) size)) (if (zero? i) (-static results (dec size)) (-value results (dec i)))
                 (>= i 0) (do
                            #_(prn "rep..." (nth splits i) (child-matcher data size (nth splits i)))
                            (let [res (child-matcher data size (nth splits i))]
                              (cond
                                res (recur (inc i) (conj splits (inc (-end res))) (assoc results i res))
                                ;(zero? i) nil
                                (>= i min) (if (zero? i) (-static results (dec (nth splits i))) (-value results (dec i)))
                                (pos? i) (recur (dec i) (pop splits) (if (contains? results i) (pop results) results))))))))))))


   (defn ? [r] (repeat 0 1 r))
   (defn * [r] (repeat 0 ##Inf r))
   (defn + [r] (repeat 1 ##Inf r))

   (defn validator [re]
     (let [matcher (-matcher re)]
       (clojure.core/fn [x]
         (if (sequential? x)
           (let [x (vec x), size (count x)]
             (boolean (some-> (matcher x size 0) (-end) (= (dec size)))))))))

   (defn validate [re data] ((validator re) data))

   (defn parser [re]
     (let [matcher (-matcher re)]
       (clojure.core/fn [x]
         (if (sequential? x)
           (let [x (vec x)
                 size (count x)
                 result (matcher x size 0)]
             (if (and result (= (-end result) (dec size)))
               (-result result)))))))

   (defn parse [re data] ((parser re) data))

   (defn describer [re]
     (let [matcher (-matcher re)]
       (clojure.core/fn [x]
         (let [x (vec x)]
           (if-let [m (matcher x (count x) 0)]
             {:result (-result m)
              :rest (subvec x (inc (-end m)))})))))

   (defn describe [re data] ((describer re) data)))

;;;; Combinators

(def ^:private anonymous (object-array 0))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))

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
  (park! [self thunk])
  (pop-thunk! [self]))

(defprotocol ^:private IExplanationDriver
  (value-path [self pos])
  (fail! [self pos errors*])
  (latest-errors [self]))

(defprotocol ^:private IParseDriver
  (succeed-with! [self v])
  (success-result [self]))

(defprotocol RegexParser
  (-validate! [self driver coll k])
  (-explain! [self driver pos coll k]))

(defn encode-item [valid? encode]
  (fn [_ coll* coll k]
    (when (seq coll)
      (let [v (first coll)]
        (when (valid? v)
          (k (conj coll* (encode v)) (rest coll)))))))

(defn decode-item [decode valid?]
  (fn [_ coll* coll k]
    (when (seq coll)
      (let [v (decode (first coll))]
        (when (valid? v)
          (k (conj coll* v) (rest coll)))))))

(defn end [-error schema path]
  (reify RegexParser
    (-validate! [_ _ coll k] (when (empty? coll) (k coll)))

    (-explain! [_ driver pos coll k]
      (if (seq coll)
        (fail! driver pos (list (-error path (value-path driver pos) schema (first coll) ::input-remaining)))
        (k pos coll)))))

(defn transformer-end [_ coll* coll k] (when (empty? coll) (k coll* coll)))

(def epsilon
  (reify RegexParser
    (-validate! [_ _ coll k] (k coll))
    (-explain! [_ _ pos coll k] (k pos coll))))

(defn transformer-epsilon [_ coll* coll k] (k coll* coll))

(defn- entry->regex [?kr] (if (vector? ?kr) (get ?kr 1) ?kr))

(defn cat
  ([] epsilon)
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (reify RegexParser
       (-validate! [_ driver coll k]
         (-validate! r driver coll
                     (fn [coll] (-validate! r* driver coll k))))

       (-explain! [_ driver pos coll k]
         (-explain! r driver pos coll
                    (fn [pos coll] (-explain! r* driver pos coll k)))))))
  ([?kr ?kr* & ?krs] (reduce cat (cat ?kr ?kr*) ?krs)))

(defn transformer-cat
  ([] transformer-epsilon)
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver coll* coll k]
       (r driver coll* coll
          (fn [coll* coll] (r* driver coll* coll k))))))
  ([?kr ?kr* & ?krs] (reduce transformer-cat (transformer-cat ?kr ?kr*) ?krs)))

(defn alt
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (reify RegexParser
       (-validate! [_ driver coll k]
         (park! driver #(-validate! r* driver coll k))      ; remember fallback
         (-validate! r driver coll k))

       (-explain! [_ driver pos coll k]
         (park! driver #(-explain! r* driver pos coll k))   ; remember fallback
         (-explain! r driver pos coll k)))))
  ([?kr ?kr* & ?krs] (reduce alt (alt ?kr ?kr*) ?krs)))

(defn transformer-alt
  ([?kr] (entry->regex ?kr))
  ([?kr ?kr*]
   (let [r (entry->regex ?kr), r* (entry->regex ?kr*)]
     (fn [driver coll* coll k]
       (park! driver #(r* driver coll* coll k))             ; remember fallback
       (r driver coll* coll k))))
  ([?kr ?kr* & ?krs] (reduce transformer-alt (transformer-alt ?kr ?kr*) ?krs)))

(defn ? [p] (alt p epsilon))

(defn transformer-? [p] (transformer-alt p transformer-epsilon))

(defn repeat [min max p]
  (reify RegexParser
    (-validate! [_ driver coll k]
      (letfn [(compulsories [coll n]
                (if (< n min)
                  (-validate! p driver coll
                              (fn [coll] (park! driver #(compulsories coll (inc n))))) ; TCO
                  (optionals coll n)))
              (optionals [coll n]
                (if (< n max)
                  (do
                    (park! driver #(k coll))                ; remember fallback
                    (-validate! p driver coll
                                (fn [coll] (park! driver #(optionals coll (inc n)))))) ; TCO
                  (k coll)))]
        (compulsories coll 0)))

    (-explain! [_ driver pos coll k]
      (letfn [(compulsories [pos coll n]
                (if (< n min)
                  (-explain! p driver pos coll
                             (fn [pos coll] (park! driver #(compulsories pos coll (inc n))))) ; TCO
                  (optionals coll n)))
              (optionals [pos coll n]
                (if (< n max)
                  (do
                    (park! driver #(k coll))                ; remember fallback
                    (-explain! p driver pos coll
                               (fn [pos coll] (park! driver #(optionals pos coll (inc n)))))) ; TCO
                  (k coll)))]
        (compulsories pos coll 0)))))

(defn transformer-repeat [min max p]
  (fn [driver coll* coll k]
    (letfn [(compulsories [coll* coll n]
              (if (< n min)
                (p driver coll* coll
                   (fn [coll] (park! driver #(compulsories coll* coll (inc n))))) ; TCO
                (optionals coll* coll n)))
            (optionals [coll* coll n]
              (if (< n max)
                (do
                  (park! driver #(k coll* coll))            ; remember fallback
                  (p driver coll
                     (fn [coll* coll] (park! driver #(optionals coll* coll (inc n)))))) ; TCO
                (k coll* coll)))]
      (compulsories coll* coll 0))))

(defn * [p]
  (reify RegexParser
    (-validate! [self driver coll k]
      (park! driver #(k coll))                              ; remember fallback
      (-validate! p driver coll
                  (fn [coll] (park! driver #(-validate! self driver coll k))))) ; TCO

    (-explain! [self driver pos coll k]
      (park! driver #(k pos coll))                          ; remember fallback
      (-explain! p driver pos coll
                 (fn [pos coll] (park! driver #(-explain! self driver pos coll k))))))) ; TCO

(defn transformer-* [p]
  (fn *p [driver coll* coll k]
    (park! driver #(k coll* coll))                          ; remember fallback
    (p driver coll* coll
       (fn [coll* coll] (park! driver #(*p driver coll* coll k)))))) ; TCO

(defn + [p] (cat p (* p)))

(defn transformer-+ [p] (transformer-cat p (transformer-* p)))

(deftype ^:private ValidationDriver [^:unsynchronized-mutable ^boolean success, ^java.util.ArrayDeque stack]
  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (park! [_ thunk] (.push stack thunk))
  (pop-thunk! [_] (when-not (.isEmpty stack) (.pop stack))))

(defn validator [-error schema path p]
  (let [p (cat p (end -error schema path))]
    (fn [coll]
      (and (sequential? coll)
           (let [driver (ValidationDriver. false (java.util.ArrayDeque.))]
             (-validate! p driver coll (fn [_] (succeed! driver)))
             (or (succeeded? driver)
                 (loop []
                   (if-some [thunk (pop-thunk! driver)]
                     (do
                       (thunk)
                       (or (succeeded? driver) (recur)))
                     false))))))))

(deftype ^:private ExplanationDriver
  [^:unsynchronized-mutable ^boolean success, ^java.util.ArrayDeque stack
   in, ^:unsynchronized-mutable errors-max-pos, ^:unsynchronized-mutable errors]

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (park! [_ thunk] (.push stack thunk))
  (pop-thunk! [_] (when-not (.isEmpty stack) (.pop stack)))

  IExplanationDriver
  (value-path [_ pos] (conj in pos))
  (fail! [_ pos errors*]
    (cond
      (> pos errors-max-pos) (do
                               (set! errors-max-pos pos)
                               (set! errors errors*))
      (= pos errors-max-pos) (set! errors (into errors errors*))))
  (latest-errors [_] errors))

(defn explainer [-error schema path p]
  (let [p (cat p (end -error schema path))]
    (fn [coll in errors]
      (if (sequential? coll)
        (let [pos 0
              driver (ExplanationDriver. false (java.util.ArrayDeque.) in pos [])]
          (-explain! p driver pos coll (fn [_ _] (succeed! driver)))
          (if (succeeded? driver)
            errors
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) errors (recur)))
                (into errors (latest-errors driver))))))
        (conj errors (-error path in schema coll :malli.core/invalid-type))))))

(deftype ^:private ParseDriver
  [^:unsynchronized-mutable ^boolean success, ^java.util.ArrayDeque stack
   ^:unsynchronized-mutable result]

  Driver
  (succeed! [_] (set! success (boolean true)))
  (succeeded? [_] success)
  (park! [_ thunk] (.push stack thunk))
  (pop-thunk! [_] (when-not (.isEmpty stack) (.pop stack)))

  IParseDriver
  (succeed-with! [self v] (succeed! self) (set! result v))
  (success-result [_] result))

(defn transformer [p]
  (let [p (transformer-cat p transformer-end)]
    (fn [coll]
      (if (sequential? coll)
        (let [driver (ParseDriver. false (java.util.ArrayDeque.) nil)]
          (p driver [] coll (fn [coll* _] (succeed-with! driver coll*)))
          (if (succeeded? driver)
            (success-result driver)
            (loop []
              (if-some [thunk (pop-thunk! driver)]
                (do
                  (thunk)
                  (if (succeeded? driver) (success-result driver) (recur)))
                coll))))
        coll))))
