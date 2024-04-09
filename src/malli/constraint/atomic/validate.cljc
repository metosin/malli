(ns malli.constraint.atomic.validate
  (:require ;[malli.core :as-alias m] ;fails in cljs
            [malli.impl.util :as miu]))

(def path-elements
  #{:get :keys :vals :first :nth :count :class})

(defn -build-path [path constraint-opts options]
  (prn "-build-path" path)
  (if (empty? path)
    identity
    (let [-build-path #(-build-path % constraint-opts options)
          [p & path] path]
      (case (when (vector? p) (first p))
        :get (let [[k d] (subvec p 1)]
               (comp (-build-path path)
                     #(get % k d)))
        (miu/-fail! ::unknown-path-element {:path-element p})))))

(defn- -is []
  (fn [{:keys [constraint constraint-validator] :as constraint-opts}
       {:malli.core/keys [schema validator] :as options}]
    (let [all (subvec constraint 1)
          _ (when-not (<= 1 (count all))
              (miu/-fail! ::is-constraint-takes-at-least-one-child {:constraint constraint}))
          path (pop all)
          ?schema (peek all)
          s (schema ?schema)
          p (cond->> (validator s)
              (= :is-not (first constraint)) (comp not))
          path (some-> path rseq (-build-path constraint-opts options))]
      (cond-> p
        path (comp path)))))

(defn validators []
  {:any (fn [{:keys [constraint]} _]
          (let [[v :as all] (subvec constraint 1)
                _ (when-not (#{[] [true]} all)
                    (miu/-fail! ::any-constraint-takes-no-children {:constraint constraint}))]
            any?))
   ;; TODO {:< [:x :y]}
   ;; TODO [:< :x :y]
   ;; TODO [:< [:nth 0] [:nth 3]]
   ;; TODO [:< [:get :x] [:get :y]]
   ;; TODO [:< [:in [:get :x]] [:in [:get :y]]]

   ;;TODO [:is SCHEMA ID PATH]
   ;:is (-is)
   ;:is-not (-is)
   })

#_
(m/all [x y]
       [:=> {:is [:int x [:get :x]]}
        [:map
         {:and wip}]])
