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
  [[{:keys [form-depth in-macro?] :as _ctx} operation* type*] command-seq]
  (let [depth form-depth]
    (concat (action->commands :begin-form
              :form-depth depth :type type* :op operation* :in-macro? in-macro?)
            command-seq
            (action->commands :end-form
              :form-depth depth :type type* :op operation* :in-macro? in-macro?))))

;; ------------------------------------------------------------------------------------------------
;; Generators: 'Callable' Primitives (run/compile time)
;; ------------------------------------------------------------------------------------------------

(defn handle-invoke
  [{:keys [form->commands] :as ctx} form]
  (let [args (rest form)]
    (with-form-wrappers [ctx (first form) :fn]
      (with-stack-frame
        (concat (mapcat form->commands args)
                (action->commands :invoke-fn
                  :op (first form) :arg-count (count args)))))))

(defn handle-do
  [{:keys [form->commands] :as ctx} form]
  (with-form-wrappers [ctx 'do :special]
    (with-implicit-do
      (mapcat form->commands (rest form)))))

(defn handle-macro
  [{:keys [->commands] :as ctx} form]
  (let [form* (cons (first form)
                    (map #(if (instance? IObj %)
                            (vary-meta % assoc ::pt/macro-arg true)
                            %)
                         (rest form)))]
    (with-form-wrappers [ctx (first form) :macro]
      (->commands (assoc ctx :in-macro? true) (macroexpand form*)))))

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

(defn wrap-check-macro
  [handler]
  (fn [ctx form]
    (let [ctx* (if (::pt/macro-arg (meta form))
                 (assoc ctx :in-macro? false)
                 ctx)]
      (handler ctx* form))))

(defn wrap-form-depth
  [handler]
  (fn [ctx form]
    (handler (update ctx :form-depth inc) form)))

(defn wrap-middleware
  [handler]
  (-> handler
      wrap-form-to-commands
      wrap-form-depth
      wrap-check-macro))

;; ------------------------------------------------------------------------------------------------
;; Generators: Handler Mappings
;; ------------------------------------------------------------------------------------------------

;; todo: need to add support for the following then update test cases:
;; -- (quote) which needs to turn off evaluation
;; -- literals like maps {}, vectors [], sets #{}, and quoted lists '()
(def form-handlers
  {;; todo: come back later and fix (def), don't need it yet for passing tests
   ;; 'def             (wrap-middleware handle-def)
   'do              (wrap-middleware handle-do)
   'fn              (wrap-middleware handle-fn)
   'fn*             (wrap-middleware handle-fn)
   ;; 'clojure.core/fn (wrap-middleware handle-fn)
   'if              (wrap-middleware handle-if)
   'let             (wrap-middleware handle-let)
   'let*            (wrap-middleware handle-let)
   'loop            (wrap-middleware handle-loop)
   'recur           (wrap-middleware handle-recur)
   'throw           (wrap-middleware handle-throw)
   'try             (wrap-middleware handle-try-catch-finally)
   'catch           (constantly nil)
   'finally         (constantly nil)
   :type/macro      (wrap-middleware handle-macro)
   :type/list       (wrap-middleware handle-invoke)
   :type/cons       (wrap-middleware handle-invoke)})

;; ------------------------------------------------------------------------------------------------
;; Generators: Form Conversion -> Commands
;; ------------------------------------------------------------------------------------------------

(defn ->commands
  [context form]
  (if-not (coll? form)
    (seq (action->commands :scalar :form form))
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
   ;; TODO: replace recur-idx with an AtomicInteger since it is not consistently
   ;; being incremented everywhere
   ;; NOTE: Could just use an Atom and the return val of swap! so this is cross-platform
   ;; compatible in other Clojure dialects
   #_#_:next-recur-idx (ptu/counter-fn)
   :recur-idx 0
   :in-macro? false})

(defn create-commands
  ([form]
   (create-commands form nil))
  ([form args]
   (create-commands form args (ns-name *ns*)))
  ([form args ns-sym]
   (assert (or (nil? args) (vector? args)) "args must be a vector or nil")
   (assert (symbol? ns-sym) "ns-sym must be a symbol")
   (let [attrs (cond-> (new-generator-ctx)
                 true (assoc :ns-sym ns-sym)
                 true (assoc :enable-cmd-gen? (not (boolean args)))
                 args (assoc :args args))]
     (->commands attrs form))))
