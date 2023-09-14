(ns lambeaux.paper-trail.seq
  (:require [lambeaux.paper-trail.core :as core]
            [paper.trail :as-alias pt]))

(def temp-ctx {::pt/current-ns *ns*})

(defn pre-process
  [ctx]
  (loop [{:keys [forms] [exp & exprs] :input :as ctx*} ctx]
    (cond
      (nil? exp) ctx*
      :else (recur (assoc ctx*
                          :input exprs
                          :forms (conj forms exp))))))

(defn trace-seq
  ([form scope]
   (trace-seq
    (pre-process {:input (tree-seq seqable? seq form)
                  :forms (list)
                  :args (list)
                  :scope scope})))
  ([ctx]
   (loop [{:keys [args scope] [exp & exprs] :forms :as ctx*} ctx]
     (cond
       (nil? exp)
       nil
       (and (symbol? exp)
            (contains? scope exp))
       (recur (assoc ctx*
                     :forms exprs
                     :args (conj args (scope exp))))
       (list? exp)
       (let [exp-size (count exp)
             result (apply (core/->impl temp-ctx (first exp))
                           (map (core/map-impl-fn temp-ctx)
                                (rest (take exp-size args))))]
         (cons {:form (take exp-size args) :result result}
               (lazy-seq
                (trace-seq (assoc ctx*
                                  :forms (cons result exprs)
                                  :args (drop exp-size args))))))
       :else
       (recur (assoc ctx* :forms exprs :args (conj args exp)))))))
