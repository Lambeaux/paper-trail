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

(defn try-resolve [{:keys [current-ns] :as _ctx} obj]
  (cond
    (qualified-symbol? obj) (resolve obj)
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

(defn handle-list
  [{:keys [reports] :as ctx} [verb & args :as in-form]]
  (add-log :handle-list in-form)
  (cond
    ;; ---- Special verbs ----
    (= verb 'defn) (handle-form ctx (last args))
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
    (throw (ex-info "Unknown handle-list case"
                    (assoc ctx :form in-form)))))

(defn handle-form
  [{:keys [arg-map] :as ctx} form]
  (add-log :handle-form form)
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
    :else (throw (ex-info "Unknown handle-form case"
                          (assoc ctx :form form)))))

(defn trace* [fn-name-or-var & args]
  (let [fn-var (if (var? fn-name-or-var)
                 fn-name-or-var
                 (resolve fn-name-or-var))
        fn-meta (meta fn-var)
        ;; ** TODO support all arglists, not just first
        fn-arglist (first (:arglists fn-meta))
        arg-map (into {} (map-indexed (fn [i v] [v (nth args i)])
                                      fn-arglist))
        fn-source (binding [*read-eval* false]
                    ;; ** TODO improve support for #() inline functions
                    ;; reading them causes them to become impls (untraceable)
                    ;; ** Maybe pre-process the code str and sub out with 
                    ;; (fn [] (...)) forms?
                    (read-string (repl/source-fn (symbol fn-var))))
        initial-ctx {:reports (atom [])
                     :current-ns (:ns fn-meta)
                     :fn-var fn-var
                     :fn-meta fn-meta
                     :fn-arglist fn-arglist
                     :arg-map arg-map
                     :fn-source fn-source}
        {:keys [reports value]} (handle-form initial-ctx fn-source)]
    (assoc {} :source fn-source :trace @reports :result value)))
