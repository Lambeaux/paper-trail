;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.generator
  (:require [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj]))

(defn code-form?
  [form sym]
  (and (sequential? form)
       (= sym (first form))))

(defn catch-form?
  [form]
  (code-form? form 'catch))

(defn finally-form?
  [form]
  (code-form? form 'finally))

(defn action->command
  ([action]
   (hash-map :action action))
  ([action k v & kvs]
   (apply hash-map (concat [:action action k v] kvs))))

(defn action->commands
  ([action]
   [(action->command action)])
  ([action k v & kvs]
   [(apply action->command (concat [action k v] kvs))]))

;; ------------------------------------------------------------------------------------------------
;; Generators: Command Wrappers
;; ------------------------------------------------------------------------------------------------

(defn with-stack-frame
  [command-seq]
  (concat (action->commands :stack-push-frame)
          command-seq
          (action->commands :stack-pop-frame)))

(defn with-implicit-do
  ([command-seq]
   (with-implicit-do true command-seq))
  ([convey-result? command-seq]
   (with-stack-frame (concat command-seq
                             (action->commands :invoke-do :convey-result? convey-result?)))))

(defn with-form-wrappers
  "Wraps a seq of commands with the :begin-form and :end-form commands."
  [[{:keys [form-depth form-meta in-macro?] :as _ctx} operation* type*] command-seq]
  (let [depth form-depth
        op* (when (symbol? operation*) operation*)]
    (concat (action->commands :begin-form
              :op op*
              :type type*
              :form-meta form-meta
              :form-depth depth
              :in-macro? in-macro?)
            command-seq
            (action->commands :end-form
              :op op*
              :type type*
              :form-meta form-meta
              :form-depth depth
              :in-macro? in-macro?))))

;; ------------------------------------------------------------------------------------------------
;; Generators: 'Callable' Primitives (run/compile time)
;; ------------------------------------------------------------------------------------------------

(defn handle-with-eval
  ([op-sym context form]
   (handle-with-eval op-sym :special context form))
  ([op-sym op-type {:keys [form->commands] :as ctx} [_ & args :as _form]]
   (with-form-wrappers [ctx op-sym op-type]
     (with-stack-frame
       (concat (mapcat form->commands (seq args))
               (action->commands :invoke-eval
                 :op op-sym
                 :arg-count (count args)))))))

(def handle-dot (partial handle-with-eval '.))
(def handle-new (partial handle-with-eval 'new))

(defn handle-interop-symbol
  [ctx form]
  (if (and (qualified-symbol? form)
           (Character/isUpperCase (first (namespace form))))
    ;; note: cheeky interop shortcut so I can get a release out the door for feedback ASAP
    ;;   probably not sustainable long term, but DM me if you find this and either had a laugh
    ;;   or were sorely disappointed
    (let [[ns-sym name-sym] (ptu/sym-split form)]
      (handle-dot ctx (with-meta (list '. ns-sym name-sym)
                        (meta form))))
    ;; note: path for normal symbols
    (action->commands :scalar :form form :eval? true)))

(defn handle-invoke
  [{:keys [form->commands] :as ctx} [fn-sym & args :as form]]
  (if (and (qualified-symbol? fn-sym)
           (Character/isUpperCase (first (namespace fn-sym))))
    ;; note: cheeky interop shortcut so I can get a release out the door for feedback ASAP
    ;;   probably not sustainable long term, but DM me if you find this and either had a laugh
    ;;   or were sorely disappointed
    (let [[ns-sym name-sym] (ptu/sym-split fn-sym)]
      (handle-dot ctx (with-meta (apply list (concat ['. ns-sym name-sym] args))
                        (meta form))))
    ;; note: path for normal function invocation
    (with-form-wrappers [ctx fn-sym :fn]
      (with-stack-frame
        (concat (mapcat form->commands (seq form))
                (action->commands :invoke-fn
                  :op (when (symbol? fn-sym) fn-sym)
                  :arg-count (count args)))))))

(defn handle-do
  [{:keys [form->commands] :as ctx} form]
  (with-form-wrappers [ctx 'do :special]
    (with-implicit-do
      (mapcat form->commands (rest form)))))

(defn handle-def
  [{:keys [form->commands ns-sym] :as ctx} form]
  (let [[name-sym & more-args :as args] (rest form)]
    (with-form-wrappers [ctx 'def :special]
      (with-stack-frame
        (concat (action->commands :scalar
                  :eval? false
                  :form (symbol (name ns-sym) (name name-sym)))
                (mapcat form->commands more-args)
                (action->commands :invoke-def :arg-count (count args)))))))

(defn handle-macro
  [{:keys [->commands] :as ctx} form]
  (let [form* (cons (first form)
                    (map #(if (instance? IObj %)
                            (vary-meta % assoc ::pt/macro-arg true)
                            %)
                         (rest form)))]
    (with-form-wrappers [ctx (first form) :macro]
      (->commands (assoc ctx :in-macro? true) (macroexpand form*)))))

(defn handle-collection-literal
  [type-kw {:keys [form->commands] :as ctx} form]
  (with-form-wrappers [ctx type-kw :literal]
    (with-stack-frame
      (let [coll-fn (case type-kw
                      :type/map    'hash-map
                      :type/set    'hash-set
                      :type/vector 'vector)]
        (concat (action->commands :scalar
                  :form coll-fn
                  :eval? true)
                (mapcat form->commands
                        (if-not (= type-kw :type/map)
                          (seq form)
                          (apply concat (seq form))))
                (action->commands :invoke-fn
                  :op coll-fn
                  :arg-count (count form)))))))

(defn handle-quote
  [ctx [_quote-sym inner-form]]
  (with-form-wrappers [ctx 'quote :special]
    (action->commands :scalar
      :form inner-form
      :eval? false)))

(defn handle-var
  [{:keys [ns-sym] :as ctx} [_var-sym inner-form]]
  (assert (symbol? inner-form)
          (str "var expects a symbol but got " (class inner-form)))
  ;; (println "[...] Handle Var: ns-sym = " ns-sym " inner-form = " inner-form)
  ;; (println "[...] Handle Var: ns-qualified = " (ptu/ns-qualify-name ns-sym inner-form))
  (with-form-wrappers [ctx 'var :special]
    (with-stack-frame
      (concat (action->commands :scalar
                :form 'find-var
                :eval? true)
              (action->commands :scalar
                :form (ptu/ns-qualify-name ns-sym inner-form)
                :eval? false)
              (action->commands :invoke-fn
                :op 'find-var
                :arg-count 1)))))

;; ------------------------------------------------------------------------------------------------
;; Generators: Special Forms
;; ------------------------------------------------------------------------------------------------

(defn handle-if
  [{:keys [form->commands] :as ctx} form]
  ;; TODO Investigate: if the repl remains alive for weeks, will we run out of gensyms?
  (let [id (gensym "if-")
        cond-pos (form->commands (first (drop 2 form)))
        cond-neg (form->commands (second (drop 2 form)))]
    (with-form-wrappers [ctx 'if :special]
      (concat (form->commands (second form))
              (action->commands :capture-state :state-id id)
              (action->commands :exec-when :state-id id :count (count cond-pos))
              cond-pos
              (when cond-neg
                (action->commands :skip-when :state-id id :count (count cond-neg)))
              cond-neg))))

(defn handle-let
  [{:keys [form->commands in-macro?] :as ctx} form]
  (let [bindings (partition 2 (second form))
        body (drop 2 form)]
    ;; :op (first form) ??
    (with-form-wrappers [ctx 'let :special]
      (concat (mapcat (fn [[bname bform]]
                        (with-stack-frame
                          (concat (form->commands bform)
                                  (action->commands :bind-name
                                    :bind-id bname :bind-from :call-stack :in-macro? in-macro?))))
                      bindings)
              (with-implicit-do
                (mapcat form->commands body))
              (mapcat (fn [[bname _]]
                        (action->commands :unbind-name
                          :bind-id bname :in-macro? in-macro?))
                      bindings)))))

(defn handle-loop
  [{:keys [->commands argdef-stack recur-idx in-macro?] :as ctx} form]
  ;; TODO: Need to double check this "stack local" form of incrementing recur-idx,
  ;; it means some idxs will be reused down the road (but it appears to be working just fine)
  (let [recur-idx (inc recur-idx)
        bindings (partition 2 (second form))
        body (drop 2 form)
        argdefs (into [] (map first bindings))
        ctx* (assoc ctx
                    :recur-idx recur-idx
                    :argdef-stack (conj argdef-stack argdefs))]
    (with-form-wrappers [ctx 'loop :special]
      (concat (mapcat (fn [[bname bform]]
                        (with-stack-frame
                          (concat (->commands ctx* bform)
                                  (action->commands :bind-name
                                    :bind-id bname
                                    :bind-from :call-stack
                                    :in-macro? in-macro?))))
                      bindings)
              (action->commands :recur-target :idx recur-idx :in-macro? in-macro?)
              (with-implicit-do
                (mapcat (partial ->commands ctx*) body))
              (mapcat (fn [[bname _]]
                        (action->commands :unbind-name :bind-id bname :in-macro? in-macro?))
                      bindings)))))

(defn handle-recur
  [{:keys [form->commands argdef-stack recur-idx in-macro?] :as ctx} form]
  (let [forms (rest form)
        ;; TODO Investigate: if the repl remains alive for weeks, will we run out of gensyms?
        state-keys (repeatedly (count forms) (fn [] (gensym "recur-")))]
    (with-form-wrappers [ctx 'recur :special]
      (concat (mapcat (fn [k form*]
                        (concat (form->commands form*)
                                (action->commands :capture-state
                                  :state-id k :in-macro? in-macro?)))
                      state-keys
                      forms)
              (mapcat (fn [k argdef]
                        [(action->command :unbind-name :bind-id argdef :in-macro? in-macro?)
                         (action->command :bind-name
                           :bind-id argdef
                           :bind-from :state
                           :state-id k
                           :in-macro? in-macro?)])
                      state-keys
                      (peek argdef-stack))
              (action->commands :replay-commands :idx recur-idx :in-macro? in-macro?)))))

;; ------------------------------------------------------------------------------------------------
;; Generators: (fn ... ) Special Form
;; ------------------------------------------------------------------------------------------------

(defn fnform->metainf
  [form]
  (let [nil-body (list 'do nil)
        parse-body (fn [sig]
                     (when-not (vector? (first sig))
                       (throw (IllegalArgumentException.
                               (str "Invalid fn sig: " sig))))
                     (hash-map
                      :arity (count (first sig))
                      :argdefs (first sig)
                      :body (if (= 1 (count sig))
                              nil-body
                              (cons 'do
                                    (map #(if (nil? %) nil-body %)
                                         (if (and (map? (second sig))
                                                  (> (count sig) 2))
                                           (drop 2 sig)
                                           (drop 1 sig)))))))
        params (rest form)
        params (if (symbol? (first params))
                 (rest params)
                 params)]
    (if (vector? (first params))
      {(count (first params)) (parse-body params)}
      (into {} (map (fn [sig]
                      [(count (first sig)) (parse-body sig)])
                    params)))))

(defn load-fn-commands
  [{:keys [->commands argdef-stack recur-idx in-macro?] :as ctx} metainf]
  (reduce (fn [accum k]
            (let [{:keys [argdefs body]} (get accum k)
                  ctx* (assoc ctx :argdef-stack (conj argdef-stack argdefs))
                  commands* (concat (map (fn [k argdef]
                                           (action->command :bind-name
                                             :bind-id argdef
                                             :bind-from :state
                                             :state-id k
                                             :in-macro? in-macro?))
                                         (map #(str "arg-" %) (range (count argdefs)))
                                         argdefs)
                                    (action->commands :recur-target :idx recur-idx :in-macro? in-macro?)
                                    (with-implicit-do (->commands ctx* body)))]
              (update accum k #(assoc % :commands commands*))))
          metainf
          (keys metainf)))

(defn handle-fn
  [ctx form]
  (let [arity->metainf (load-fn-commands ctx (fnform->metainf form))]
    (with-form-wrappers [ctx 'fn :special]
      (action->commands :create-fn :arities arity->metainf))))

;; ------------------------------------------------------------------------------------------------
;; Generators: (try ... ) / (throw ...) Special Forms
;; ------------------------------------------------------------------------------------------------

(defn handle-throw
  [{:keys [form->commands] :as ctx} form]
  (assert (= 2 (count form))
          "Invalid arguments to throw, expects single throwable instance")
  (with-form-wrappers [ctx 'throw :special]
    (with-stack-frame
      (concat (form->commands (second form))
              (action->commands :invoke-throw)))))

(defn catch->cmds
  [{:keys [form->commands in-macro?] :as ctx} ex-sym body]
  (with-form-wrappers [ctx 'catch :special]
    (concat (action->commands :bind-name
              :bind-id ex-sym :bind-from :state :state-id :caught-ex :in-macro? in-macro?)
            (with-implicit-do
              (mapcat form->commands body))
            (action->commands :unbind-name :bind-id ex-sym :in-macro? in-macro?))))

;; Note: (finally) uses an implicit (do), but does not convey-result? of the (do) since
;; (finally) is only for executing side effects and not impacting return value
(defn finally->cmds
  [{:keys [form->commands] :as ctx} body]
  (with-form-wrappers [ctx 'finally :special]
    (concat (action->commands :begin-finally)
            (action->commands :set-context :props {:is-finally? true})
            (with-implicit-do false
              (mapcat form->commands body))
            (action->commands :set-context :props {:is-finally? false})
            (action->commands :end-finally))))

(defn handle-try-catch-finally
  [{:keys [form->commands in-macro?] :as ctx} form]
  (let [args (rest form)
        finally* (let [finally? (last args)]
                   (when (finally-form? finally?) finally?))
        args*   (if-not finally* args (butlast args))
        body    (take-while (complement catch-form?) args*)
        catches (drop-while (complement catch-form?) args*)
        id      (gensym "try-")]
    (with-form-wrappers [ctx 'try :special]
      (concat (action->commands :setup-try
                :id id
                :catches (mapv (fn [[op clazz-sym ex-sym & body]]
                                 (assert (= op 'catch) "Only catch statements allowed here")
                                 ;; TODO: consolidate where symbol resolution occurs so that
                                 ;;       commands can remain serializable.
                                 ;; TODO: instead of 'resolve, use 'ns-resolve and pass in the
                                 ;;       ns-sym from the context.
                                 (hash-map :ex-class (resolve clazz-sym)
                                           :commands (catch->cmds ctx ex-sym body)))
                               catches)
                :finally (when finally*
                           (finally->cmds ctx (rest finally*)))
                :in-macro? in-macro?)
              (with-implicit-do (mapcat form->commands body))
              (action->commands :cleanup-try :id id :in-macro? in-macro?)))))

;; ------------------------------------------------------------------------------------------------
;; Generators: Misc / Old / Outdated / Incomplete
;; ------------------------------------------------------------------------------------------------

;; todo: need to revisit this 'def logic, haven't tested it in awhile, might be garbage
;; in general, not sure if in-fn def support is a priority
;; note: did not add 'with-form-wrappers calls to handle-def
#_(defn handle-def
    [{:keys [form->commands argdef-stack recur-idx in-macro? enable-cmd-gen? args] :as attrs} form]
    (if enable-cmd-gen?
      (concat [{:cmd :begin-form :type :special :op 'def :in-macro? in-macro?}]
              (when (> (count form) 2)
                (form->commands attrs (last form)))
              [{:cmd :intern-var
                :var-id (second form)
                :has-root-binding? (> (count form) 2)
                :in-macro? in-macro?}
               {:cmd :end-form :type :special :op 'def :in-macro? in-macro?}])
      (if-not (and (> (count form) 2)
                   (or (= 'fn (first (last form)))
                       (= 'fn* (first (last form)))
                       (= 'clojure.core/fn (first (last form)))))
        (throw (IllegalArgumentException. (str "Provided var is not a fn: " form)))
        (let [{:keys [argdefs body]} (get (fnform->metainf (last form)) (count args))]
          (concat (map (fn [argdef arg]
                         {:cmd :bind-name
                          :bind-id argdef
                          :bind-from :command-value
                          :value arg
                          :in-macro? false})
                       argdefs
                       args)
                  [{:cmd :recur-target :idx recur-idx :in-macro? false}]
                  (form->commands
                   (assoc attrs :enable-cmd-gen? true :argdef-stack (conj argdef-stack argdefs))
                   body))))))

;; ------------------------------------------------------------------------------------------------
;; Generators: Middleware
;; ------------------------------------------------------------------------------------------------

(defn wrap-form-to-commands
  [handler]
  (fn [{:keys [->commands] :as ctx} form]
    (handler (assoc ctx :form->commands (partial ->commands ctx))
             form)))

(defn wrap-form-details
  [handler]
  (fn [ctx form]
    (-> ctx
        (assoc :form-meta (meta form))
        (update :form-depth inc)
        (handler form))))

(defn wrap-check-macro
  [handler]
  (fn [ctx form]
    (let [ctx* (if-not (::pt/macro-arg (meta form))
                 ctx
                 (assoc ctx :in-macro? false))]
      (handler ctx* form))))

(defn wrap-middleware
  [handler]
  (-> handler
      wrap-form-to-commands
      wrap-form-details
      wrap-check-macro))

;; ------------------------------------------------------------------------------------------------
;; Generators: Handler Mappings
;; ------------------------------------------------------------------------------------------------

(def form-handlers
  {'do               (wrap-middleware handle-do)
   'fn               (wrap-middleware handle-fn)
   'fn*              (wrap-middleware handle-fn)
   'def              (wrap-middleware handle-def)
   ;; 'clojure.core/fn (wrap-middleware handle-fn)
   'if               (wrap-middleware handle-if)
   'let              (wrap-middleware handle-let)
   'let*             (wrap-middleware handle-let)
   'loop             (wrap-middleware handle-loop)
   'loop*            (wrap-middleware handle-loop)
   'recur            (wrap-middleware handle-recur)
   'throw            (wrap-middleware handle-throw)
   'try              (wrap-middleware handle-try-catch-finally)
   'catch            (constantly nil)
   'finally          (constantly nil)
   'var              (wrap-middleware handle-var)
   'quote            (wrap-middleware handle-quote)
   ;; -------------------------------------------------------------------------
   ;; Handled with eval (for now, will need to be improved later)
   ;; -------------------------------------------------------------------------
   '.                (wrap-middleware handle-dot)
   'new              (wrap-middleware handle-new)
   'set!             (wrap-middleware (partial handle-with-eval 'set!))
   'monitor-enter    (wrap-middleware (partial handle-with-eval 'monitor-enter))
   'monitor-exit     (wrap-middleware (partial handle-with-eval 'monitor-exit))
   ;; -------------------------------------------------------------------------
   :type/symbol-qual (wrap-middleware handle-interop-symbol)
   :type/map         (wrap-middleware (partial handle-collection-literal :type/map))
   :type/vector      (wrap-middleware (partial handle-collection-literal :type/vector))
   :type/set         (wrap-middleware (partial handle-collection-literal :type/set))
   :type/macro       (wrap-middleware handle-macro)
   :type/list        (wrap-middleware handle-invoke)
   :type/cons        (wrap-middleware handle-invoke)})

;; ------------------------------------------------------------------------------------------------
;; Generators: Form Conversion -> Commands
;; ------------------------------------------------------------------------------------------------

(defn ->commands
  [context form]
  (if-not (coll? form)
    (if-let [h (get form-handlers (ptu/classify form))]
      (lazy-seq (h context form))
      (seq (action->commands :scalar :form form :eval? true)))
    (let [h (or (get form-handlers (first form))
                (get form-handlers (ptu/classify form)))]
      (if h
        (lazy-seq (h context form))
        (seq (action->commands :not-implemented :form form))))))

(defn new-generator-ctx
  []
  {:->commands ->commands
   ;; form->commands will be added with middleware
   :form->commands nil
   :argdef-stack (list)
   :form-depth -1
   :form-meta nil
   ;; TODO: replace recur-idx with an AtomicInteger since it is not consistently
   ;; being incremented everywhere
   ;; NOTE: Could just use an Atom and the return val of swap! so this is cross-platform
   ;; compatible in other Clojure dialects
   #_#_:next-recur-idx (ptu/counter-fn)
   :ns-sym nil
   :recur-idx 0
   :in-macro? false})

(defn generate
  ([form]
   (generate nil form))
  ([ns-sym form]
   (when ns-sym
     (assert (symbol? ns-sym) "ns-sym must be a symbol"))
   (let [context (new-generator-ctx)
         ns-sym* (if ns-sym
                   ns-sym
                   (ns-name *ns*))]
     (->commands (assoc context :ns-sym ns-sym*) form))))
