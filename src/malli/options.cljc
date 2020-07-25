(ns malli.options
  (:refer-clojure :exclude [type])
  #?(:clj (:import (clojure.lang IDeref))))

#?(:cljs (goog-define type "default")
   :clj  (def type (as-> (or (System/getProperty "malli.options/type") "default") $ (.intern $))))

(defn options [x] (reify IDeref (#?(:cljs cljs.core/-deref, :clj deref) [_] (if (map? x) x (deref x)))))

(def ^:private options* (atom nil))

(defn update-default-options! [& args]
  (if-not (identical? type "default")
    (apply swap! options* args)
    (throw (ex-info (str "can't set default options with type " (pr-str type)) {:type type})))
  (options options*))

(defn reset-custom-default-options! [options]
  (update-default-options! (constantly options)))
