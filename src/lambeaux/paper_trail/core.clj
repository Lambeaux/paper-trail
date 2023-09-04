(ns lambeaux.paper-trail.core
  (:require [paper.trail :as-alias pt]
            [lambeaux.paper-trail.samples :as s]
            [clojure.pprint :as pretty]
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

(def clojure-core-symbols->vars
  (ns-publics (the-ns 'clojure.core)))

(defn core-fn? [obj]
  (contains? clojure-core-symbols->vars obj))

(defn to-namespace
  "Attempts to convert an object to a namespace,
   returning the namespace if successful, or nil
   otherwise. Will convert symbols to namespaces."
  [obj]
  (try
    (the-ns obj)
    (catch Exception _e nil)))

(defn try-resolve
  "Attempts to resolve the provided object to a var.
   Supports full qualifications using the full name
   or an alias."
  [{::pt/keys [current-ns] :as _ctx} obj]
  (cond
    (qualified-symbol? obj) (let [obj-ns (-> obj
                                             namespace
                                             symbol)]
                              (if (to-namespace obj-ns)
                                (resolve obj)
                                (ns-resolve (-> current-ns
                                                ns-aliases
                                                (get obj-ns)
                                                (to-namespace))
                                            (symbol (name obj)))))
    (core-fn? obj) (resolve obj)
    :else (ns-resolve current-ns obj)))

(defn accessible-macro? [ctx obj]
  (try
    (when-let [resolved (try-resolve ctx obj)]
      (boolean (:macro (meta resolved))))
    (catch Exception _e false)))

(defn accessible-fn? [ctx obj]
  (try
    (when-let [resolved (try-resolve ctx obj)]
      (fn? @resolved))
    (catch Exception _e false)))

(defn ->impl [ctx sym]
  @(try-resolve ctx sym))

(defn terminal? [obj]
  (or (keyword? obj)
      (number? obj)
      (boolean? obj)
      (nil? obj)
      (char? obj)
      (string? obj)))

(defn handle-defn
  [{::pt/keys [arglist] :as ctx} in-form]
  (let [[a b c :as forms] (reverse in-form)]
    (if (or (and (list? a) (vector? b))
            (and (list? a) (map? b) (vector? c)))
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
      (accessible-macro? ctx verb)
      (handle-list ctx (spy :expanded (macroexpand in-form)))
      (accessible-fn? ctx verb)
      (let [form (doall (cons verb
                              (->> (spy :args args)
                                   (map (partial handle-form ctx))
                                   (map :value))))
            result (apply (->impl ctx verb)
                          (map #(if (accessible-fn? ctx %) (->impl ctx %) %)
                               (rest form)))]
        (add-log :form form)
        (add-log :result result)
        (swap! reports conj {:form form :result result})
        (spy :ctx (assoc ctx :value result)))
    ;; ---- Catch all ----
      :else
      (throw (ex-info "Unknown handle-list case" (assoc ctx :err-form in-form))))
    (catch clojure.lang.ExceptionInfo ei (throw ei))
    (catch Exception e (throw (ex-info "Interpreter error" (assoc ctx :err-form in-form) e)))))

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
      (accessible-fn? ctx form) (assoc ctx :value form)
      (symbol? form) (assoc ctx :value (spy :argmap (arg-map form)))
      (terminal? form) (assoc ctx :value form)
      :else (throw (ex-info "Unknown handle-form case" (assoc ctx :err-form form))))
    (catch clojure.lang.ExceptionInfo ei (throw ei))
    (catch Exception e (throw (ex-info "Interpreter error" (assoc ctx :err-form form) e)))))

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

(defn setup-args
  [fn-args arglist fn-arg-map]
  (cond
    (= '& (first arglist)) (assoc fn-arg-map (second arglist) (seq fn-args))
    (empty? arglist) fn-arg-map
    :else (recur (rest fn-args)
                 (rest arglist)
                 (assoc fn-arg-map (first arglist) (first fn-args)))))

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
           ::pt/arg-map (setup-args fn-args arglist {}))))

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
