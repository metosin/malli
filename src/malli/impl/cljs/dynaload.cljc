(ns malli.impl.cljs.dynaload)

(defmacro dynaload [[_quote s]]
  `(LazyVar.
    (fn []
      (if (cljs.core/exists? ~s)
        ~(vary-meta s assoc :cljs.analyzer/no-resolve true)
        (throw
         (js/Error.
          (str "Var " '~s " does not exist, "
               (namespace '~s) " never required")))))
    nil))
