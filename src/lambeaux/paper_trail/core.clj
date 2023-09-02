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

;; need a better way to determine if something is in
;; clojure core proper, maybe by using the namespace?
;; or generate a set of vars/symbols?
(def core-fn? #{'map 'filter 'remove 'into})

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

(defn accessible-macro? [obj]
  (try
    (when-let [resolved (resolve obj)]
      (boolean (:macro (meta resolved))))
    (catch Exception _e false)))

(defn accessible-fn? [obj]
  (try
    (when-let [resolved (resolve obj)]
      (fn? @resolved))
    (catch Exception _e false)))

(defn ->impl [sym]
  @(resolve sym))

(defn handle-list
  [{:keys [reports] :as ctx} [verb & args :as in-form]]
  (add-log :handle-list in-form)
  (cond
    ;; ---- special verbs ----
    (= verb 'defn) (handle-form ctx (last args))
    ;; need to eval fn* because its not in clojure core
    (= verb 'fn*) (assoc ctx :value (eval (spy :fn* in-form)))
    ;; ---- verb is known and invokable ----
    (accessible-macro? verb)
    (handle-list ctx (spy :expanded (macroexpand in-form)))
    (accessible-fn? verb)
    (let [form (doall (cons verb
                            (->> (spy :args args)
                                 (map (partial handle-form ctx))
                                 (map :value))))
          result (apply (->impl verb)
                        (map #(if (accessible-fn? %) (->impl %) %)
                             (rest form)))]
      (add-log :form form)
      (add-log :result result)
      (swap! reports conj {:form form :result result})
      (spy :ctx (assoc ctx :value result)))
    ;; ---- catch all ----
    :else
    (throw (ex-info "Unknown handle-list case"
                    (assoc ctx :form in-form)))))

(defn handle-form
  [{:keys [arg-map] :as ctx} form]
  (add-log :handle-form form)
  (cond
    (list? form) (handle-list ctx form)
    (instance? clojure.lang.Cons form) (handle-list ctx form)
    (accessible-fn? form) (assoc ctx :value form)
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
                    (read-string (repl/source-fn (symbol fn-var))))
        initial-ctx {:reports (atom [])
                     :fn-var fn-var
                     :fn-meta fn-meta
                     :fn-arglist fn-arglist
                     :arg-map arg-map
                     :fn-source fn-source}
        {:keys [reports value]} (handle-form initial-ctx fn-source)]
    (assoc {} :source fn-source :trace @reports :result value)))
