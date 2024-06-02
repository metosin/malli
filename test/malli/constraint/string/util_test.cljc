(ns malli.constraint.string.util-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [malli.constraint.string.util :as mcsu]))

(deftest code-point-offset-seq-test
  (is (= [{:code-point 97, :char-offset 0, :code-point-offset 0}
          {:code-point 98, :char-offset 1, :code-point-offset 1}
          {:code-point 99, :char-offset 2, :code-point-offset 2}]
         (vec
           (mcsu/code-point-offset-seq
             "abc"))))
  (is (= [{:code-point 97, :char-offset 0, :code-point-offset 0}
          {:code-point 78177, :char-offset 1, :code-point-offset 1, :surrogate-pair [55308 56673]}
          {:code-point 99, :char-offset 3, :code-point-offset 2}]
         (vec
           (mcsu/code-point-offset-seq
             "a𓅡c"))))
  (is (= [97 78177 99]
         (vec
           (mcsu/code-point-seq
             "a𓅡c"))))
  (is (= "abc"
         (str/join (mapv mcsu/code-point->string
                         (mcsu/code-point-seq
                           "abc")))))
  (is (= "a𓅡c"
         (str/join (mapv mcsu/code-point->string
                         (mcsu/code-point-seq
                           "a𓅡c"))))))
