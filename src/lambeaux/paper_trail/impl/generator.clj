(ns lambeaux.paper-trail.impl.generator
  (:require [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj]))

(defn wrap-check-macro
  [handler]
  (fn [attrs form]
    (let [attrs (if (::pt/macro-arg (meta form))
                  (assoc attrs :in-macro-impl? false)
                  attrs)]
      (handler attrs form))))

(defn handle-invoke
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [args (rest form)
        cmds (mapcat (partial form->commands attrs) (reverse args))]
    (concat [{:cmd :begin-form :type :fn :op (first form) :impl? in-macro-impl?}]
            cmds
            [{:cmd :invoke-fn :op (first form) :arg-count (count args)}
             {:cmd :end-form :type :fn :op (first form) :impl? in-macro-impl?}])))

(defn handle-do
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [form->commands (partial form->commands attrs)]
    (concat [{:cmd :begin-form :type :special :op 'do :impl? in-macro-impl?}]
            (mapcat form->commands (rest form))
            [{:cmd :end-form :type :special :op 'do :impl? in-macro-impl?}])))

(defn handle-if
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  ;; TODO Investigate: if the repl remains alive for weeks, will we run out of gensyms?
  (let [id (gensym "if-")
        form->commands (partial form->commands attrs)
        cond-pos (form->commands (first (drop 2 form)))
        cond-neg (form->commands (second (drop 2 form)))]
    (concat [{:cmd :begin-form :type :special :op 'if :impl? in-macro-impl?}]
            (form->commands (second form))
            [{:cmd :capture-state :state-id id}
             {:cmd :exec-when :state-id id :count (count cond-pos)}]
            cond-pos
            (when cond-neg
              [{:cmd :skip-when :state-id id :count (count cond-neg)}])
            cond-neg
            [{:cmd :end-form :type :special :op 'if :impl? in-macro-impl?}])))

(defn handle-let
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [form->commands (partial form->commands attrs)
        bindings (partition 2 (second form))
        body (drop 2 form)]
    (concat [{:cmd :begin-form :type :special :op (first form) :impl? in-macro-impl?}]
            (mapcat (fn [[bname bform]]
                      (concat (form->commands bform)
                              [{:cmd :bind-name
                                :bind-id bname
                                :bind-from :call-stack
                                :impl? in-macro-impl?}]))
                    bindings)
            (mapcat form->commands body)
            (mapcat (fn [[bname _]]
                      [{:cmd :unbind-name :bind-id bname :impl? in-macro-impl?}])
                    bindings)
            [{:cmd :end-form :type :special :op (first form) :impl? in-macro-impl?}])))

(defn handle-macro
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [form* (cons (first form)
                    (map #(if (instance? IObj %)
                            (vary-meta % assoc ::pt/macro-arg true)
                            %)
                         (rest form)))]
    (concat [{:cmd :begin-form :type :macro :op (first form) :impl? in-macro-impl?}]
            (form->commands (assoc attrs :in-macro-impl? true)
                            (macroexpand form*))
            [{:cmd :end-form :type :macro :op (first form) :impl? in-macro-impl?}])))

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
  [{:keys [form->commands argdef-stack recur-idx in-macro-impl?]
    :as attrs} metainf]
  (reduce (fn [accum k]
            (let [{:keys [argdefs body]} (get accum k)
                  attrs* (assoc attrs :argdef-stack (conj argdef-stack argdefs))
                  commands* (concat (map (fn [k argdef]
                                           {:cmd :bind-name
                                            :bind-id argdef
                                            :bind-from :state
                                            :state-id k
                                            :impl? in-macro-impl?})
                                         (map #(str "arg-" %) (range (count argdefs)))
                                         argdefs)
                                    [{:cmd :recur-target :idx recur-idx :impl? false}]
                                    (form->commands attrs* body))]
              (update accum k #(assoc % :commands commands*))))
          metainf
          (keys metainf)))

(defn handle-fn
  [attrs form]
  (let [arity->metainf (load-fn-commands attrs (fnform->metainf form))]
    [{:cmd :create-fn :arities arity->metainf}]))

(defn handle-def
  [{:keys [form->commands argdef-stack recur-idx in-macro-impl? enable-cmd-gen? args]
    :as attrs} form]
  (if enable-cmd-gen?
    (concat [{:cmd :begin-form :type :special :op 'def :impl? in-macro-impl?}]
            (when (> (count form) 2)
              (form->commands attrs (last form)))
            [{:cmd :intern-var
              :var-id (second form)
              :has-root-binding? (> (count form) 2)
              :impl? in-macro-impl?}
             {:cmd :end-form :type :special :op 'def :impl? in-macro-impl?}])
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
                        :impl? false})
                     argdefs
                     args)
                [{:cmd :recur-target :idx recur-idx :impl? false}]
                (form->commands
                 (assoc attrs :enable-cmd-gen? true :argdef-stack (conj argdef-stack argdefs))
                 body))))))

(defn handle-loop
  [{:keys [form->commands argdef-stack recur-idx in-macro-impl? enable-cmd-gen? args]
    :as attrs} form]
  (let [recur-idx (inc recur-idx)
        bindings (partition 2 (second form))
        body (drop 2 form)
        argdefs (into [] (map first bindings))
        attrs (assoc attrs
                     :recur-idx recur-idx
                     :argdef-stack (conj argdef-stack argdefs))]
    (concat [{:cmd :begin-form :type :special :op 'loop :impl? in-macro-impl?}]
            (mapcat (fn [[bname bform]]
                      (concat (form->commands attrs bform)
                              [{:cmd :bind-name
                                :bind-id bname
                                :bind-from :call-stack
                                :impl? in-macro-impl?}]))
                    bindings)
            [{:cmd :recur-target :idx recur-idx :impl? in-macro-impl?}]
            (mapcat #(form->commands attrs %) body)
            (mapcat (fn [[bname _]]
                      [{:cmd :unbind-name :bind-id bname :impl? in-macro-impl?}])
                    bindings)
            [{:cmd :end-form :type :special :op 'loop :impl? in-macro-impl?}])))

(defn handle-recur
  [{:keys [form->commands argdef-stack recur-idx in-macro-impl? enable-cmd-gen? args]
    :as attrs} form]
  (concat [{:cmd :begin-form :type :special :op 'recur :impl? in-macro-impl?}]
          (let [form->commands (partial form->commands attrs)
                forms (rest form)
                ;; TODO Investigate: if the repl remains alive for weeks, will we run out of gensyms?
                state-keys (repeatedly (count forms) (fn [] (gensym "recur-")))]
            (concat (mapcat (fn [k form*]
                              (concat (form->commands form*)
                                      [{:cmd :capture-state :state-id k :impl? in-macro-impl?}]))
                            state-keys
                            forms)
                    (mapcat (fn [k argdef]
                              [{:cmd :unbind-name :bind-id argdef :impl? in-macro-impl?}
                               {:cmd :bind-name
                                :bind-id argdef
                                :bind-from :state
                                :state-id k
                                :impl? in-macro-impl?}])
                            state-keys
                            (peek argdef-stack))
                    [{:cmd :replay-commands :idx recur-idx :impl? in-macro-impl?}]))
          [{:cmd :end-form :type :special :op 'recur :impl? in-macro-impl?}]))

(defn catch->cmds
  [{:keys [form->commands in-macro-impl?] :as attrs} ex-sym body]
  (let [form->commands (partial form->commands attrs)]
    (concat [{:cmd :begin-form :type :special :op 'catch :impl? in-macro-impl?}
             {:cmd :bind-name :bind-id ex-sym :bind-from :call-stack :impl? in-macro-impl?}]
            (mapcat form->commands body)
            [{:cmd :unbind-name :bind-id ex-sym :impl? in-macro-impl?}
             {:cmd :end-form :type :special :op 'catch :impl? in-macro-impl?}])))

(defn finally->cmds
  [{:keys [form->commands in-macro-impl?] :as attrs} body]
  (let [form->commands (partial form->commands attrs)]
    (concat [{:cmd :begin-form :type :special :op 'finally :impl? in-macro-impl?}
             {:cmd :begin-finally}
             {:cmd :set-context :props {:is-finally? true}}]
            (mapcat form->commands body)
            [{:cmd :set-context :props {:is-finally? false}}
             {:cmd :end-finally}
             {:cmd :end-form :type :special :op 'finally :impl? in-macro-impl?}])))

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

(defn handle-try-catch-finally
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [form->commands (partial form->commands attrs)
        args (rest form)
        finally* (let [finally? (last args)]
                   (when (finally-form? finally?) finally?))
        args*   (if-not finally* args (butlast args))
        body    (take-while (complement catch-form?) args*)
        catches (drop-while (complement catch-form?) args*)
        id      (gensym "try-")]
    (concat [{:cmd :begin-form :type :special :op 'try :impl? in-macro-impl?}
             {:cmd :setup-try
              :id id
              :catches (mapv (fn [[op clazz-sym ex-sym & body]]
                               (assert (= op 'catch)
                                       "Only catch statements allowed here")
                               ;; TODO: consolidate where symbol resolution occurs so that
                               ;;       commands can remain serializable.
                               ;; TODO: instead of 'resolve, use 'ns-resolve and pass in the
                               ;;       ns-sym from the context.
                               (hash-map :ex-class (resolve clazz-sym)
                                         :commands (catch->cmds attrs ex-sym body)))
                             catches)
              :finally (when finally*
                         (finally->cmds attrs (rest finally*)))
              :impl? in-macro-impl?}]
            (mapcat form->commands body)
            [{:cmd :cleanup-try :id id :impl? in-macro-impl?}
             {:cmd :end-form :type :special :op 'try :impl? in-macro-impl?}])))

(defn handle-throw
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (assert (= 2 (count form))
          "Invalid arguments to throw, expects single throwable instance")
  (let [cmds (form->commands attrs (second form))]
    (concat [{:cmd :begin-form :type :special :op 'throw :impl? in-macro-impl?}]
            cmds
            [{:cmd :invoke-throw}
             {:cmd :end-form :type :special :op 'throw :impl? in-macro-impl?}])))

;; todo: need to add support for the following then update test cases:
;; -- (quote) which needs to turn off evaluation
;; -- literals like maps {}, vectors [], sets #{}, and quoted lists '()
(def form-handlers
  {'def             (wrap-check-macro handle-def)
   'do              (wrap-check-macro handle-do)
   'fn              (wrap-check-macro handle-fn)
   'fn*             (wrap-check-macro handle-fn)
   ;; 'clojure.core/fn (wrap-check-macro handle-fn)
   'if              (wrap-check-macro handle-if)
   'let             (wrap-check-macro handle-let)
   'let*            (wrap-check-macro handle-let)
   'loop            (wrap-check-macro handle-loop)
   'recur           (wrap-check-macro handle-recur)
   'throw           (wrap-check-macro handle-throw)
   'try             (wrap-check-macro handle-try-catch-finally)
   'catch           (constantly nil)
   'finally         (constantly nil)
   :type/list-fn    (wrap-check-macro handle-invoke)
   :type/list-macro (wrap-check-macro handle-macro)})

(defn form->commands
  [attrs form]
  (if-not (coll? form)
    (seq [{:cmd :scalar :form form}])
    (let [h (or (get form-handlers (first form))
                (get form-handlers (ptu/classify form)))]
      (if h
        (lazy-seq (h attrs form))
        (seq [{:cmd :not-implemented :form form}])))))

(defn new-form-ctx
  []
  {:form->commands form->commands
   :argdef-stack (list)
   ;; TODO: replace recur-idx with an AtomicInteger since it is not consistently
   ;; being incremented everywhere
   ;; NOTE: Could just use an Atom and the return val of swap! so this is cross-platform
   ;; compatible in other Clojure dialects
   :recur-idx 0
   :in-macro-impl? false})

(defn create-commands
  ([form]
   (create-commands form nil))
  ([form args]
   (create-commands form args (ns-name *ns*)))
  ([form args ns-sym]
   (assert (or (nil? args) (vector? args)) "args must be a vector or nil")
   (assert (symbol? ns-sym) "ns-sym must be a symbol")
   (let [attrs (cond-> (new-form-ctx)
                 true (assoc :ns-sym ns-sym)
                 true (assoc :enable-cmd-gen? (not (boolean args)))
                 args (assoc :args args))]
     (form->commands attrs form))))
