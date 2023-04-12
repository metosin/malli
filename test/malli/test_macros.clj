(ns malli.test-macros)

(defmacro when-env [s & body]
  (when (System/getenv s)
    `(do ~@body)))
