(ns lambeaux.paper-trail.decomp
  (:require [lambeaux.paper-trail.core :as core]
            [lambeaux.paper-trail.seq :as pts]
            [clojure.set :as set]
            [paper.trail :as-alias pt])
  (:import [clojure.lang IObj Cons]))

(defn decompose-form
  [form]
  (cond (vector? form)
        {:event :vector :children (map decompose-form form)}
        (sequential? form)
        (let [args (rest form)]
          {:event :invoke
           :op (first form)
           :form form
           :arg-count (count args)
           :children (map decompose-form args)})
        :else
        {:event :scalar :value form}))

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

(defn fndef->bodies
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
      (let [{:keys [argdefs body]} (get (fndef->bodies (last form)) (count args))]
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
    (concat [{:cmd :begin-form :type :special :op 'finally :impl? in-macro-impl?}]
            (mapcat form->commands body)
            [{:cmd :end-form :type :special :op 'finally :impl? in-macro-impl?}])))

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

(def form-handlers
  {'def             (wrap-check-macro handle-def)
   'do              (wrap-check-macro handle-do)
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
                (get form-handlers (pts/classify form)))]
      (if h
        (lazy-seq (h attrs form))
        (seq [{:cmd :not-implemented :form form}])))))

(defn create-commands
  ([form]
   (form->commands {:form->commands form->commands
                    :argdef-stack (list)
                    :recur-idx 0
                    :in-macro-impl? false
                    :enable-cmd-gen? true}
                   form))
  ([form args]
   (assert (vector? args) "args must be a vector")
   (form->commands {:form->commands form->commands
                    :argdef-stack (list)
                    :recur-idx 0
                    :in-macro-impl? false
                    :enable-cmd-gen? false
                    :args args}
                   form)))

;; ----------------------------------------------------------------------------------------

(comment
  "TODO (Later): Come back and revisit some cases regarding 'apply'"
  ;; does not use varadic
  (apply update-in [{:k []}
                    [:k 0]
                    (fn [m & args] (apply assoc m [:x 1 :args args]))]))

(defn with-keyvals
  [& kvs]
  (fn [m] (apply assoc m kvs)))

(defn update-fn-ctx
  [{:keys [fn-idx] :as ctx} f & args]
  (apply update-in (concat [ctx [:fn-stack fn-idx] f] args)))

(defn next-command
  [{:keys [command-history]
    [cmd & cmds] :commands
    :as ctx}]
  (update-fn-ctx ctx (with-keyvals
                       :commands cmds
                       :command-history (conj command-history cmd))))

(defn default-update
  ([ctx kvs]
   (default-update ctx true kvs))
  ([ctx consume-command? kvs]
   (cond-> ctx
     consume-command? next-command
     true (update-fn-ctx (apply with-keyvals kvs)))))

(defn ctx-find-catch
  ;; TODO: need an implementation here
  []
  ())

(def temp-ctx {::pt/current-ns *ns*})

;; ----------------------------------------------------------------------------------------

(defn process-no-op
  [ctx]
  (next-command ctx))

(defn process-invoke-fn
  [{:keys [enable-try-catch-support? call-stack]
    [cmd & _] :commands
    :as ctx}]
  (let [arg-count (:arg-count cmd)
        [ex? result] (try
                       [false (apply (core/->impl temp-ctx (:op cmd))
                                     (map (core/map-impl-fn temp-ctx)
                                          (take arg-count call-stack)))]
                       ;; JVM Note: typical applications should not catch throwable
                       ;; if you copy this pattern, make sure you know what you're doing
                       ;; (catch Throwable t [true t])
                       ;; TODO: eventually support Throwable for more accurate results
                       (catch Exception e [true e]))
        result (if (instance? IObj result)
                 (with-meta result {::pt/is-evaled? true})
                 result)
        _final-form (map (fn [arg]
                           (or (and (instance? IObj arg)
                                    (::pt/raw-form (meta arg)))
                               arg))
                         (take arg-count call-stack))]
    (when (and ex? (not enable-try-catch-support?))
      (throw result))
    (-> ctx
        (assoc :throwing? ex?)
        (default-update
         [:call-stack (conj (apply list (drop arg-count call-stack))
                            result)]))))

(defn process-invoke-throw
  [{:keys [call-stack]
    [_cmd & _] :commands
    :as _ctx}]
  ;; TODO: don't actually throw, resolve the ctx against the stack and try-handlers
  (throw (first call-stack)))

(defn process-scalar
  [{:keys [call-stack source-scope impl-scope]
    [cmd & _] :commands
    :as ctx}]
  (let [scalar-form (:form cmd)
        scalar-value (or (peek (get source-scope scalar-form))
                         (peek (get impl-scope scalar-form))
                         scalar-form)
        scalar-value (case scalar-value
                       ::pt/nil nil
                       ::pt/false false
                       scalar-value)]
    (default-update ctx [:call-stack (conj call-stack scalar-value)])))

;; ----------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
  (default-update ctx [:state (assoc state (:state-id cmd) (peek call-stack))]))

(defn process-bind-name
  [{:keys [state call-stack source-scope impl-scope]
    [{:keys [bind-id bind-from impl? value state-id]} & _] :commands
    :as ctx}]
  (let [val-to-bind  (case bind-from
                       :command-value value
                       :call-stack (peek call-stack)
                       :state (get state state-id))
        val-to-bind  (case val-to-bind
                       nil ::pt/nil
                       false ::pt/false
                       val-to-bind)
        keyval-pairs (if impl?
                       [:impl-scope (update impl-scope bind-id #(conj % val-to-bind))]
                       [:source-scope (update source-scope bind-id #(conj % val-to-bind))])]
    (default-update ctx keyval-pairs)))

(defn process-unbind-name
  [{:keys [source-scope impl-scope]
    [{:keys [bind-id impl?]} & _] :commands
    :as ctx}]
  (let [keyvals (if impl?
                  [:impl-scope (update impl-scope bind-id pop)]
                  [:source-scope (update source-scope bind-id pop)])]
    (default-update ctx keyvals)))

;; ----------------------------------------------------------------------------------------

(defn keyvals-for-exec
  [{:keys [command-history]
    [cmd & cmds] :commands
    :as _ctx}]
  [:commands cmds
   :command-history (conj command-history cmd)])

(defn keyvals-for-skip
  [{:keys [command-history]
    [cmd & cmds] :commands
    :as _ctx}]
  [:commands (drop (:count cmd) cmds)
   :command-history (into (conj command-history cmd)
                          (take (:count cmd) cmds))])

(defn process-exec-when
  [{:keys [state]
    [cmd & _] :commands
    :as ctx}]
  (let [keyvals (if (get state (:state-id cmd))
                  (keyvals-for-exec ctx)
                  (keyvals-for-skip ctx))]
    (default-update ctx false keyvals)))

(defn process-skip-when
  [{:keys [state]
    [cmd & _] :commands
    :as ctx}]
  (let [keyvals (if (get state (:state-id cmd))
                  (keyvals-for-skip ctx)
                  (keyvals-for-exec ctx))]
    (default-update ctx false keyvals)))

(defn process-replay-commands
  [{:keys [command-history]
    [{:keys [idx] :as cmd} & cmds] :commands
    :as ctx}]
  (let [cmds-to-replay (take-while (fn [old-cmd]
                                     (not (and (= :recur-target (:cmd old-cmd))
                                               (= idx (:idx old-cmd)))))
                                   command-history)]
    (default-update ctx false
                    [:commands (concat (reverse cmds-to-replay) [cmd] cmds)
                     :command-history (drop (count cmds-to-replay) command-history)])))

;; ----------------------------------------------------------------------------------------

(defn process-setup-try
  [{:keys [fn-idx try-handlers] :as ctx}]
  (let [{[cmd] :commands :as _fctx} (get-in ctx [:fn-stack fn-idx])]
    (assoc ctx :try-handlers
           (conj try-handlers
                 ;; TODO: need an ID for the specific (try) block
                 (merge (select-keys ctx [:fn-idx])
                        (select-keys cmd [:id :catches :finally]))))))

;; ----------------------------------------------------------------------------------------

(defn wrap-throwing
  [handler]
  (fn [ctx]
    (if (:throwing? ctx)
      (process-no-op ctx)
      (handler ctx))))

(defn wrap-check-not-infinite
  [handler]
  (fn [{:keys [fn-idx] :as ctx}]
    (let [ctx* (handler ctx)
          pre-handler-cmds (get-in ctx [:fn-stack fn-idx :commands])
          post-handler-cmds (get-in ctx* [:fn-stack fn-idx :commands])]
      (if-not (= pre-handler-cmds post-handler-cmds)
        ctx*
        (throw (new AssertionError
                    "Infinite processing error detected: handler did not update commands"))))))

(defn wrap-convenience-mappings
  [handler]
  (fn [{:keys [fn-idx] :as ctx}]
    (let [fctx (get-in ctx [:fn-stack fn-idx])]
      (assert (empty? (set/intersection (into #{} (keys ctx))
                                        (into #{} (keys fctx))))
              "There must be no key overlap between ctx and fctx")
      (apply dissoc
             (handler (merge ctx fctx))
             (keys fctx)))))

(defn wrap-uncaught-ex
  [handler]
  (fn [ctx]
    (try
      (handler ctx)
      (catch Exception e
        (throw (ex-info "Uncaught interpreter exception" {:ctx ctx} e))))))

(defn wrap-command-middleware
  ([handler]
   (wrap-command-middleware handler false))
  ([handler wrap-throwing?]
   (cond-> handler
     wrap-throwing? wrap-throwing
     true           wrap-check-not-infinite
     true           wrap-convenience-mappings
     true           wrap-uncaught-ex)))

(defn inject-command-middleware
  [all-handlers]
  (reduce (fn [m k]
            (update m k wrap-command-middleware))
          all-handlers
          (keys all-handlers)))

"TODO: When 'throwing?' -- we only care about:
 ** unbind-name
 ** cleanup-try
 
 Alternate between processing the two so that intermediate (finally)
 forms and catch/re-throws are scoped correctly as you work your way
 back up the fn call stack."

(defn process-test-hook
  [ctx]
  (let [f (:hook-fn (first (:commands ctx)))]
    (f ctx)))

(def command-handlers
  (inject-command-middleware
   {:begin-form      process-no-op
    :end-form        process-no-op
    :invoke-fn       process-invoke-fn
    :invoke-throw    process-invoke-throw
    :scalar          process-scalar
    :capture-state   process-capture-state
    :bind-name       process-bind-name
    :unbind-name     process-unbind-name
    :exec-when       process-exec-when
    :skip-when       process-skip-when
    :replay-commands process-replay-commands
    :setup-try       process-setup-try
    :cleanup-try     process-no-op
    ;; --------------------------------------------------------------------------
    :intern-var      process-no-op ;; TODO: fix
    :recur-target    process-no-op ;; TODO: fix
    ;; --------------------------------------------------------------------------
    :test-hook       process-test-hook
    :not-implemented process-scalar}))

(comment
  "Example use case for test hook: "
  (let [f (fn [{:keys [fn-idx] :as ctx}]
            (-> ctx
                next-command
                (assoc-in [:fn-stack fn-idx :throwing?] true)))
        test-hook {:cmd :test-hook :hook-fn f}
        cmds (create-commands '(+ 1 1))
        cmds* (concat (butlast cmds)
                      [test-hook (last cmds)])]
    (execute cmds*)))

(defn new-fn-ctx
  [commands]
  {:commands commands
   :command-history (list)
   :call-stack (list)
   :source-scope {}
   :impl-scope {}
   :state {}})

(defn new-exec-ctx
  [commands]
  {:fn-idx 0
   :fn-stack [(new-fn-ctx commands)]
   :try-handlers (list)
   :throwing? false
   :enable-try-catch-support? false})

(defn execute
  [commands]
  (loop [{:keys [fn-idx] :as ctx} (new-exec-ctx commands)]
    (let [{:keys [commands call-stack] :as fctx} (get-in ctx [:fn-stack fn-idx])
          h (get command-handlers (:cmd (first commands)))]
      (if h
        (recur (h ctx))
        (first call-stack)))))

(defn ctx-seq
  [input]
  (cond
    (seq? input)
    (ctx-seq (new-exec-ctx input))
    (map? input)
    (cons input (lazy-seq
                 (let [fctx (get-in input [:fn-stack (:fn-idx input)])
                       h (get command-handlers
                              (:cmd (first (:commands fctx))))]
                   (when h
                     (ctx-seq (h input))))))
    :else
    (throw (IllegalArgumentException. "Input must be seq or map"))))

;; ----------------------------------------------------------------------------------------

(defn run-eval
  ([form]
   (run-eval form nil))
  ([form args]
   (let [cmds (if args
                (create-commands form args)
                (create-commands form))]
     (execute cmds))))

(defn run-debug-report
  ([form]
   (run-debug-report form nil))
  ([form args]
   (run-debug-report form args identity))
  ([form args xform]
   (let [cmds (if args
                (create-commands form args)
                (create-commands form))]
     (->> cmds
          (ctx-seq)
          (map (fn [{:keys [fn-idx throwing?] :as ctx}]
                 (-> (get-in ctx [:fn-stack fn-idx])
                     (assoc :throwing? throwing?))))
          (map (fn [{:keys [commands] :as ctx}]
                 (-> ctx
                     (assoc :next-command (first commands))
                     (dissoc :command-history :commands))))
          (map (fn [ctx]
                 (update-vals ctx #(if-not (instance? IObj %)
                                     %
                                     (with-meta % {:portal.viewer/default
                                                   :portal.viewer/pprint})))))
          (xform)
          (into (with-meta [] {:portal.viewer/default
                               :portal.viewer/table}))))))
