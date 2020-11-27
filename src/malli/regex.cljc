(ns malli.regex
  (:refer-clojure :exclude #_[fn cat repeat ? * +] [+ * repeat cat compile])
  #?(:clj (:import [clojure.lang MapEntry]
                   [java.util Arrays])))

;;; FIXME: Check that entire seq is consumed
;;; TODO: clojure.spec.alpha/spec equivalent

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

;;;; Compiler

(def ^:private label-ops
  #{:label :jump :fork> :fork<})

(def ^:private op->opcode
  (zipmap (conj label-ops :pred :save0 :save1) (range)))

(defmacro ^:private asm [& exprs]
  (let [gen (gensym 'gen)
        exprs (partition 2 exprs)]
    `(let [~gen (memoize gensym)]
       (concat
         ~@(map
             (fn [[op arg]]
               (let [op (keyword op)]
                 (if (contains? op->opcode op)
                   [op (if (contains? label-ops op)
                         `(~gen ~(keyword arg))
                         arg)]
                   (do
                     (assert (= op :include))
                     arg))))
             exprs)))))

(defn- patch
  "Resolve labels to offsets.
  (idempotent)"
  [instructions]
  (let [[insts labels] (reduce (fn [[insts labels] [op arg]]
                                 (if (= :label op)
                                   [insts (assoc labels arg (quot (count insts) 2))]
                                   [(conj insts op arg) labels]))
                               [[] {}] (partition 2 instructions))
        code-length (quot (count insts) 2)]
    (loop [pc 0, [op arg & rinsts] insts, insts []]
      (if (< pc code-length)
        (let [arg (if (contains? label-ops op)
                    (if-some [dest (get labels arg)]
                      (- dest pc)
                      arg)
                    arg)]
          (recur (inc pc) rinsts (conj insts op arg)))
        insts))))

(deftype CompiledPattern [opcodes args])

(defn- codegen [instructions]
  (let [inst-count (quot (count instructions) 2)
        bytecodes (byte-array inst-count)
        args (object-array inst-count)]
    (loop [pc 0, [op arg & rinsts] instructions]
      (if (< pc inst-count)
        (do
          (aset bytecodes pc (byte (doto (get op->opcode op) assert)))
          (aset args pc arg)
          (recur (inc pc) rinsts))
        (CompiledPattern. bytecodes args)))))

(defn compile [r] (-> r patch codegen))

;;;; Combinators

(def ^:private anonymous (object-array 0))

(defn -tagged [k v]
  #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))

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

(defmacro ^:private opcode-case [op & body]
  (letfn [(convert-pat [pat]
            (if (symbol? pat)
              (doto (get op->opcode (keyword pat)) assert)
              (map convert-pat pat)))]
    `(case ~op
       ~@(->> body
              (partition 2)
              (mapcat (fn [[pat expr]] [(convert-pat pat) expr]))))))

(defn- make-bitset ^bytes [max-val]
  (byte-array (quot (dec (clojure.core/+ max-val 8)) 8)))

(defn- clear-bitset! [^bytes set] (Arrays/fill set (byte 0)))

(defn- bitset-set! [^bytes set, ^long v]
  (let [byte-index (quot v 8)
        bit-index (rem v 8)]
    (aset set byte-index (unchecked-byte (bit-or (aget set byte-index)
                                                 (bit-shift-left 1 bit-index))))
    nil))

(defn- bitset-contains? [^bytes set, ^long v]
  (let [byte-index (quot v 8)
        bit-index (rem v 8)]
    (= (bit-and (bit-shift-right (aget set byte-index) bit-index)
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
  clojure.lang.APersistentVector
  (save0 [stack ->frame start] (conj stack (->frame start)))
  (save1 [stack k buf _]
    ;; 'return' `node`:
    (let [callee-frame (peek stack)
          node (-children callee-frame k buf)
          stack (pop stack)]
      ;; Update 'caller' state:
      (update stack (dec (count stack)) -add-match k node)))
  (fetch [stack buf _] (-children (peek stack) nil buf)))

(defprotocol ^:private IVMState
  (thread-count [state])
  (fork! [state pc matches])
  (truncate! [state]))

(deftype ^:private VMState [^:unsynchronized-mutable ^long nthreads pcs matchess]
  IVMState
  (thread-count [_] nthreads)

  (fork! [_ pc matches]
    (let [i nthreads]
      (set! nthreads (inc i))
      (aset ^longs pcs i ^long pc)
      (aset ^"[Ljava.lang.Object;" matchess i matches)))

  (truncate! [_] (set! nthreads 0)))

(definterface ^:private VM
  (clear_visited [])
  (match_item [^long pos, coll, buf, ^malli.regex.VMState state, ^malli.regex.VMState state*])
  (add_thread [^long pos, buf, ^malli.regex.VMState state, ^long pc, matches])
  (final_errors []))

(defn- add-thread! [^VM vm pos buf state pc matches]
  (.clear_visited vm)
  (.add_thread vm pos buf state pc matches))

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
                  (.add_thread self pos buf state (inc pc) matches)
                  (recur pos buf state (clojure.core/+ pc (long (aget args pc))) matches))
          fork< (do
                  (.add_thread self pos buf state (clojure.core/+ pc (long (aget args pc))) matches)
                  (recur pos buf state (inc pc) matches))

          save0 (recur pos buf state (inc pc) (save0 matches (aget args pc) pos))
          save1 (recur pos buf state (inc pc) (save1 matches (aget args pc) buf pos))

          pred (fork! state pc matches)))

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
                #_"add-thread! makes other opcodes impossible at this point"))
            matches)))))

  (final_errors [_] nil))

(deftype ^:private ExplanatoryVM [^PikeVM super, path, in, -error, ^:unsynchronized-mutable errors]
  VM
  (clear_visited [_] (.clear_visited super))

  (add_thread [_ pos buf state pc matches] (.add_thread super pos buf state pc matches))

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
                pred (if coll
                       (let [errors** (arg (first coll) in errors*)]
                         (when (identical? errors** errors*)
                           (add-thread! self (inc pos) (conj buf (first coll)) state* (inc pc) matches))
                         (recur (inc i) errors**))
                       (recur (inc i)
                              (conj errors* (-error path in #_FIXME/arg arg nil ::end-of-input))))
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
      (let [longest (or (.match_item vm pos coll buf state state*) longest)]
        (if (and (> (thread-count state*) 0) coll)
          (do
            (truncate! state)
            (recur (inc pos) (next coll) (conj buf (first coll)) state* state longest))
          (if longest
            (fetch longest buf (or coll ()))
            (.final_errors vm)))))))

(defn exec-recognizer [automaton coll]
  (exec-automaton* automaton (->vm automaton) coll sink-bank-succ))

(defn exec-explainer [automaton path coll in -error]
  (exec-automaton* automaton (->explanatory-vm automaton path in -error) coll sink-bank-fail))

(defn exec-tree-automaton [automaton coll]
  (exec-automaton* automaton (->vm automaton) coll [(start-frame 0)]))

;;;; Malli APIs

(defn parser [re]
  (let [automaton (compile re)]
    (fn [x]
      (and (sequential? x)
           (exec-tree-automaton automaton x)))))

(defn parse [re coll] ((parser re) coll))
