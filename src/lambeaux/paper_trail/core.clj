(ns lambeaux.paper-trail.core
  (:require [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail :as-alias pt]
            [clojure.repl :as repl]))

(defn reload-me []
  (require [(symbol (str *ns*))] :reload-all))

(defn trace-seq [])
(defn trace [])
(defn trace-fn [])
(defn report->log [])

;; ---------------------------------------------------------
;; Tracking
;; ---------------------------------------------------------

(def logs (atom []))

(defn add-log [label x]
  (swap! logs conj [:print label x])
  x)

(defn spy
  [label x]
  (swap! logs conj [:spy label x])
  x)

;; ---------------------------------------------------------
;; Descendent Recursive Interpreter
;; ---------------------------------------------------------

(declare handle-form)

(defn handle-defn
  [{::pt/keys [arglist] :as ctx} in-form]
  (let [[a b c :as forms] (reverse in-form)]
    (if (or (and a (vector? b))
            (and a (map? b) (vector? c)))
      (handle-form ctx a)
      (loop [[[argdefs impl] & xs] forms]
        (if (= argdefs arglist)
          (handle-form ctx impl)
          (recur xs))))))

(defn handle-list
  [{::pt/keys [reports] :as ctx} [verb & args :as in-form]]
  (add-log :handle-list in-form)
  (try
    (cond
      ;; ---- Special verbs ----
      (= verb 'defn) (handle-defn ctx in-form)
      ;; Need to eval fn* because its not in clojure core
      (= verb 'fn*) (assoc ctx :value (eval (spy :fn* in-form)))
      ;; ---- Verb is known and invokable ----
      (ptu/accessible-macro? ctx verb)
      (handle-list ctx (spy :expanded (macroexpand in-form)))
      (ptu/accessible-fn? ctx verb)
      (let [form (doall (cons verb
                              (->> (spy :args args)
                                   (map (partial handle-form ctx))
                                   (map :value))))
            result (apply (ptu/->impl ctx verb)
                          (map (ptu/map-impl-fn ctx)
                               (rest form)))]
        (add-log :form form)
        (add-log :result result)
        (swap! reports conj {:form form :result result})
        (spy :ctx (assoc ctx :value result)))
      ;; ---- Catch all ----
      :else
      (throw (ex-info "Unknown handle-list case" (assoc ctx :err-form in-form))))
    (catch clojure.lang.ExceptionInfo ei
      (throw ei))
    (catch Exception e
      (throw (ex-info "Interpreter error" (assoc ctx :err-form in-form) e)))))

(defn handle-form
  [{::pt/keys [arg-map] :as ctx} form]
  (add-log :handle-form form)
  (try
    (cond
      (list? form) (handle-list ctx form)
      (instance? clojure.lang.Cons form) (handle-list ctx form)
      (vector? form) (assoc ctx :value
                            (into [] (comp (map (partial handle-form ctx))
                                           (map :value))
                                  form))
      (map? form) (assoc ctx :value
                         (into {}
                               (map (fn [[k v]]
                                      [(:value (handle-form ctx k))
                                       (:value (handle-form ctx v))]))
                               form))
      (and (symbol? form)
           (contains? arg-map form)) (assoc ctx :value (spy :argmap (arg-map form)))
      (ptu/accessible-fn? ctx form) (assoc ctx :value form)
      (ptu/terminal? form) (assoc ctx :value form)
      :else (throw (ex-info "Unknown handle-form case" (assoc ctx :err-form form))))
    (catch clojure.lang.ExceptionInfo ei
      (throw ei))
    (catch Exception e
      (throw (ex-info "Interpreter error" (assoc ctx :err-form form) e)))))

(defn start-ctx
  [fn-name-or-var args]
  (let [fn-var (if (var? fn-name-or-var)
                 fn-name-or-var
                 (resolve fn-name-or-var))
        {:keys [ns] :as fn-meta} (meta fn-var)]
    (assoc {}
           ::pt/reports (atom [])
           ::pt/current-ns ns
           ::pt/fn-var fn-var
           ::pt/fn-var-meta fn-meta
           ::pt/fn-args args)))

(declare handle-destructure)

(defn map-destruct-fn
  [arg _argdef scope]
  (fn [[k v]]
    (cond
      (or (map? k) (vector? k)) (handle-destructure (get arg (keyword v)) k scope)
      (= "keys" (name k)) (map #(vector % (get arg (keyword (namespace k) (name %)))) v)
      (= :as k) (list [v arg])
      :else (list [k (get arg v)]))))

(defn seq-destruct
  [arg argdef scope]
  (loop [i 0 scope* scope]
    (cond
      (or (>= i (count argdef))
          (nil? (nth argdef i))) scope*
      (= :as (nth argdef i)) (recur (inc (inc i))
                                    (assoc scope* (nth argdef (inc i)) (seq arg)))
      (= '& (nth argdef i)) (recur (inc (inc i))
                                   (assoc scope* (nth argdef (inc i)) (drop i arg)))
      (or (map? (nth argdef i))
          (vector? (nth argdef i))) (recur (inc i)
                                           (handle-destructure
                                            (nth arg i) (nth argdef i) scope*))
      :else (recur (inc i)
                   (assoc scope* (nth argdef i) (nth arg i))))))

(defn handle-destructure
  [arg argdef scope]
  (cond
    (map? argdef) (into scope  (->> (seq argdef)
                                    (map (map-destruct-fn arg argdef scope))
                                    (apply concat)))
    (vector? argdef) (into scope (seq-destruct arg argdef scope))
    :else (throw (ex-info "Unrecognized destructure type"
                          (hash-map :arg arg :argdef argdef :scope scope)))))

(defn setup-ctx-with-arg-map
  [{::pt/keys [fn-args] {:keys [arglists]} ::pt/fn-var-meta :as ctx}]
  (let [arglist (if-let [arity-match (first (filter #(= (count fn-args) (count %))
                                                    arglists))]
                  arity-match
                  (->> arglists
                       (map #(vector % (count %)))
                       (apply max-key last)
                       first))]
    (assoc ctx
           ::pt/arglist arglist
           ::pt/arg-map (seq-destruct fn-args arglist {}))))

(defn setup-ctx-with-source
  [{::pt/keys [fn-var] :as ctx}]
  (assoc ctx ::pt/fn-source
         (binding [*read-eval* false]
           ;; ** TODO improve support for #() inline functions,
           ;; reading them causes them to become impls (untraceable).
           ;; ** Maybe pre-process the code str and sub out with 
           ;; (fn [] (...)) forms? Or create a wrapper obj that impls IFn
           ;; but its .toString returns the #() form?
           (read-string (repl/source-fn (symbol fn-var))))))

(defn trace* [fn-name-or-var & args]
  (let [{::pt/keys [fn-source] :as initial-ctx} (-> (start-ctx fn-name-or-var args)
                                                    setup-ctx-with-arg-map
                                                    setup-ctx-with-source)
        {::pt/keys [reports] :keys [value]} (handle-form initial-ctx fn-source)]
    (assoc {} :source fn-source :trace @reports :result value)))
