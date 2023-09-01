(ns lambeaux.paper-trail.core
  (:require [lambeaux.paper-trail.samples :as s]
            [clojure.pprint :as pretty]
            [clojure.repl :as repl]))

(defn reload-me []
  (require [(symbol (str *ns*))] :reload-all))

(defn trace-seq [])
(defn trace [])
(defn trace-fn [])
(defn report->log [])

(def core-fn? #{'map 'filter 'remove 'into})

(def logs (atom []))

(defn do-println [x]
  (swap! logs conj [:print x])
  x)

(defn spy
  ([x]
   (swap! logs conj [:spy x])
   x)
  ([label x]
   (swap! logs conj [:spy label x])
   x))

(declare handle-form)

(defn accessible-fn? [sym]
  (when-let [resolved (resolve sym)]
    (fn? @resolved)))

(defn ->fn [sym]
  @(resolve sym))

(defn handle-list
  [{:keys [reports] :as ctx} [verb & args :as in-form]]
  (do-println [:handle-list in-form])
  (flush)
  (cond
    (= verb 'defn)
    (handle-form ctx (last args))
    (= verb 'fn*)
    (assoc ctx :value (eval (spy :fn* in-form)))
    (accessible-fn? verb)
    (let [form (doall (cons verb
                            (->> (spy :args args)
                                 (map (partial handle-form ctx))
                                 (map :value))))
          _ (do-println [:form form])
          result (apply (->fn verb) (rest form))
          _ (do-println [:result result])]
      (spy :ctx (assoc ctx
                       :reports (conj reports {:form form :result result})
                       :value result)))
    :else
    (throw (ex-info "Unknown handle-list case"
                    (assoc ctx :form in-form)))))

(defn handle-form
  [{:keys [arg-map] :as ctx} form]
  (do-println [:handle-form form])
  (flush)
  (cond
    (list? form) (handle-list ctx form)
    (instance? clojure.lang.Cons form) (handle-list ctx form)
    ;; returning the fn impl here makes the src form look wonky
    ;; probably want both the symbol and fn impl
    (accessible-fn? form) (assoc ctx :value (->fn form))
    (symbol? form) (assoc ctx :value (spy :argmap (arg-map form)))
    :else (throw (ex-info "Unknown handle-form case"
                          (assoc ctx :form form)))))

(defn trace* [fn-name-or-var & args]
  (let [fn-var (if (var? fn-name-or-var)
                 fn-name-or-var
                 (resolve fn-name-or-var))
        fn-meta (meta fn-var)
        ;; TODO support all arglists, not just first
        fn-arglist (first (:arglists fn-meta))
        arg-map (into {} (map-indexed (fn [i v] [v (nth args i)])
                                      fn-arglist))
        fn-source (binding [*read-eval* false]
                    (read-string (repl/source-fn (symbol fn-var))))]
    (handle-form {:reports []
                  :fn-var fn-var
                  :fn-meta fn-meta
                  :fn-arglist fn-arglist
                  :arg-map arg-map
                  :fn-source fn-source} fn-source)))
