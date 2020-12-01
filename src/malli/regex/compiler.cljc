(ns malli.regex.compiler
  (:refer-clojure :exclude [compile]))

(def label-ops
  #{:label :jump :fork> :fork<})

(def op->opcode
  (zipmap (conj label-ops :pred :explain :end :save0 :save1) (range)))

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
        bytecodes (#?(:clj byte-array, :cljs make-array) inst-count)
        args (object-array inst-count)]
    (loop [pc 0, [op arg & rinsts] instructions]
      (if (< pc inst-count)
        (do
          (aset bytecodes pc (byte (doto (get op->opcode op) assert)))
          (aset args pc arg)
          (recur (inc pc) rinsts))
        (CompiledPattern. bytecodes args)))))

(defn compile [r] (-> r patch codegen))
