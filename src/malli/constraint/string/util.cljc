(ns malli.constraint.string.util)

;; https://lambdaisland.com/blog/12-06-2017-clojure-gotchas-surrogate-pairs
(defn- char-code-at [str pos]
  #?(:clj (int (.charAt ^String str pos))
     :cljs (.charCodeAt str pos)))

(defn- to-code-point [high low]
  #?(:clj (Character/toCodePoint (char high) (char low))
     :cljs (miu/-fail! ::to-code-point-not-implemented-cljs)))

(defn code-point->string [code-point]
  #?(;;https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     :clj (if (= 1 (Character/charCount (int code-point)))
            (String/valueOf (char code-point))
            (String. (Character/toChars code-point)))
     :cljs (miu/-fail! ::code-point->string-not-implemented-cljs)))

(defn code-point-offset-seq
  "Returns a lazy seq of {:code-point-offset nat, :code-point int, :char-offset nat, (optional :surrogate-pair) [high low]}"
  ([str]
   (code-point-offset-seq str 0 0))
  ([str code-point-offset char-offset]
   (lazy-seq
     (when (< char-offset (count str))
       (let [start-offset char-offset
             high (char-code-at str char-offset)
             char-offset (inc char-offset)
             low (when (and (<= 0xD800 (int high) 0xDBFF)
                            (< char-offset (count str)))
                   (char-code-at str char-offset))
             code (cond-> high low (to-code-point low))
             char-offset (cond-> char-offset low inc)]
         (cons (cond-> {:code-point (int code)
                        :char-offset start-offset
                        :code-point-offset code-point-offset}
                 low (assoc :surrogate-pair [high low]))
               (code-point-offset-seq str (inc code-point-offset) char-offset)))))))

(defn code-point-seq [str]
  (map :code-point (code-point-offset-seq str)))
