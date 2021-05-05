(ns malli.impl.NoStackException)

(gen-class
 :name malli.impl.NoStackException
 :extends clojure.lang.ExceptionInfo)

(defn -fillInStackTrace
  ([_])
  ([_ _]))
