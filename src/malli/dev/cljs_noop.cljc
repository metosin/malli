(ns malli.dev.cljs-noop
  #?(:cljs (:require-macros [malli.dev.cljs-noop])))

#?(:clj (defmacro stop! []))
#?(:clj (defmacro collect-all! []))
#?(:clj (defmacro start! ([]) ([options])))
#?(:clj (defmacro deregister-function-schemas! []))
