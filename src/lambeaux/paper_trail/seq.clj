(ns lambeaux.paper-trail.seq
  (:require [lambeaux.paper-trail.core :as core]
            [paper.trail :as-alias pt]))

(def temp-ctx {::pt/current-ns *ns*})

(defn pre-process
  [forms prev dlogs scope]
  (loop [[exp & exprs] forms
         prev* prev
         dlogs* dlogs]
    (cond
      (nil? exp) [prev* (list) dlogs* scope]
      :else (recur exprs (conj prev* exp) dlogs*))))

(defn trace-seq
  ([form scope]
   (apply trace-seq
          (pre-process
           (tree-seq seqable? seq form) (list) [] scope)))
  ([forms args dlogs scope]
   (loop [[exp & exprs] forms
          args* args
          dlogs* dlogs]
     (cond
       (nil? exp)
       nil
       (and (symbol? exp)
            (contains? scope exp))
       (recur exprs (conj args* (scope exp)) dlogs*)
       (list? exp)
       (let [exp-size (count exp)
             result (apply (core/->impl temp-ctx (first exp))
                           (map (core/map-impl-fn temp-ctx) 
                                (rest (take exp-size args*))))]
         (cons {:form (take exp-size args*) :result result}
               (lazy-seq
                (trace-seq (cons result exprs) 
                           (drop exp-size args*) 
                           dlogs* 
                           scope))))
       :else
       (recur exprs (conj args* exp) dlogs*)))))
