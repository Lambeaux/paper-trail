(ns lambeaux.paper-trail.decomp
  (:require [lambeaux.paper-trail.core :as core]
            [lambeaux.paper-trail.seq :as pts]
            [clojure.set :as set]
            [paper.trail :as-alias pt])
  (:import [clojure.lang IObj IPending LazySeq]))

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
    (concat [{:cmd :begin-form :type :special :op 'finally :impl? in-macro-impl?}
             {:cmd :set-context :props {:is-finally? true}}]
            (mapcat form->commands body)
            [{:cmd :set-context :props {:is-finally? false}}
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

(defn new-form-ctx
  []
  {:form->commands form->commands
   :argdef-stack (list)
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

(def temp-ctx {::pt/current-ns *ns*})

;; ----------------------------------------------------------------------------------------

;; (defrecord Unrealized [wrapped-value])

(deftype Unrealized [theWrappedVal theValsMetadata]
  ;; IPending (??)
  ;; (isRealized [this])
  IObj
  (meta [this] (. this theValsMetadata))
  (withMeta [this m] (new Unrealized (. this theWrappedVal) m)))

(defn val->unrealized
  [v]
  (new Unrealized v nil))

(defn unrealized->val
  [u]
  (. u theWrappedVal))

(defn realized-val?
  [val*]
  (if (instance? Unrealized val*)
    false
    (if-not (instance? IPending val*)
      true
      (realized? val*))))

(defn stack-unwrap-val
  [val*]
  (if-not (instance? Unrealized val*)
    val*
    (unrealized->val val*)))

(defn stack-peek
  [call-stack]
  (stack-unwrap-val (peek call-stack)))

(defn stack-take
  [n call-stack]
  (map stack-unwrap-val (take n call-stack)))

(defn assoc-meta
  "Like 'assoc' but operates on obj's metadata."
  ([obj k v]
   (vary-meta obj assoc k v))
  ([obj k v k* v*]
   (vary-meta obj assoc k v k* v*))
  ([obj k v k* v* & kvs]
   (let [f #(apply assoc (concat [% k v k* v*] kvs))]
     (vary-meta obj f))))

(defn stack-eval-result
  [result]
  (cond-> result
    (not (realized-val? result)) (val->unrealized)
    (instance? IObj result)      (assoc-meta ::pt/is-evaled? true)))

;; ----------------------------------------------------------------------------------------

;; TODO clean up all this messy code that concerns
;; symbol and namespace resolution
(defn map-impl-fn [ctx]
  (fn [arg]
    (cond 
      ;; needed this check because lazy ex's were getting
      ;; swallowed by accessible-fn? and ->impl
      (instance? LazySeq arg) arg
      (core/accessible-fn? ctx arg) (core/->impl ctx arg) 
      :else arg)))

(defn process-no-op
  [ctx]
  (next-command ctx))

(defn process-invoke-fn
  [{:keys [enable-try-catch-support? is-throwing? is-finally? call-stack]
    [cmd & _] :commands
    :as ctx}]
  (let [arg-count (:arg-count cmd)
        [ex? result] (try
                       (let [return (apply (core/->impl temp-ctx (:op cmd))
                                           (map (map-impl-fn temp-ctx)
                                                (stack-take arg-count call-stack)))]
                         ;; [false (if-not (seq? return) return (doall return))]
                         ;; Turns out, requiring doall here was a red herring. The real
                         ;; failure was occurring down below when trying to with-meta a
                         ;; lazy, unrealized seq. 
                         [false return])
                       ;; JVM Note: typical applications should not catch throwable
                       ;; if you copy this pattern, make sure you know what you're doing
                       ;; (catch Throwable t [true t])
                       ;; TODO: eventually support Throwable for more accurate results
                       (catch Exception e [true e]))
        #_#_final-form (map (fn [arg]
                              (or (and (instance? IObj arg)
                                       (::pt/raw-form (meta arg)))
                                  arg))
                            (stack-take arg-count call-stack))]
    (when (and ex? (not enable-try-catch-support?))
      (throw result))
    (let [set-throwing? (boolean (or is-throwing? ex?))]
      (-> ctx
          (assoc :is-throwing? set-throwing?)
          (assoc :is-finally? (if set-throwing? false is-finally?))
          (default-update
           [:call-stack (conj (apply list (drop arg-count call-stack))
                              (stack-eval-result result))])))))

(defn process-invoke-throw
  [{:keys [enable-try-catch-support? call-stack]
    :as ctx}]
  (when-not enable-try-catch-support?
    (throw (stack-peek call-stack)))
  (-> ctx
      (next-command)
      (assoc :is-throwing? true)
      (assoc :is-finally? false)))

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
    (default-update ctx [:call-stack (conj call-stack (stack-eval-result scalar-value))])))

;; ----------------------------------------------------------------------------------------

(defn process-capture-state
  [{:keys [call-stack state]
    [cmd & _] :commands
    :as ctx}]
  (default-update ctx [:state (assoc state (:state-id cmd) (stack-peek call-stack))]))

(defn process-bind-name
  [{:keys [state call-stack source-scope impl-scope]
    [{:keys [bind-id bind-from impl? value state-id]} & _] :commands
    :as ctx}]
  (let [val-to-bind  (case bind-from
                       :command-value value
                       :call-stack (stack-peek call-stack)
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

(defn find-catch
  [e catches]
  (->> catches
       (filter (fn [{:keys [ex-class]}] (instance? ex-class e)))
       (first)
       :commands))

(defn process-setup-try
  [{:keys [try-handlers]
    [cmd] :commands
    :as ctx}]
  (-> ctx
      next-command
      (assoc :try-handlers
             (conj try-handlers
                   (merge (select-keys ctx [:fn-idx])
                          (select-keys cmd [:id :catches :finally]))))))

(defn process-cleanup-try
  [{:keys [is-throwing? call-stack try-handlers]
    [_ & cmds] :commands
    :as ctx}]
  (let [{:keys [catches] finally-commands :finally} (peek try-handlers)
        catch-commands (find-catch (stack-peek call-stack) catches)]
    (-> ctx
        (assoc :is-throwing? (when is-throwing? (not (boolean catch-commands))))
        (default-update [:commands (concat catch-commands finally-commands cmds)]))))

;; ----------------------------------------------------------------------------------------

(defn wrap-throwing
  [handler]
  (let [cleanup-op? #{:unbind-name :cleanup-try}]
    (fn [{:keys [is-throwing? is-finally?] [{:keys [cmd]}] :commands :as ctx}]
      (if (and is-throwing?
               (not is-finally?)
               (not (cleanup-op? cmd)))
        (process-no-op ctx)
        (handler ctx)))))

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
   (wrap-command-middleware false handler))
  ([wrap-throwing? handler]
   (cond-> handler
     wrap-throwing? wrap-throwing
     true           wrap-check-not-infinite
     true           wrap-convenience-mappings
     true           wrap-uncaught-ex)))

(defn inject-command-middleware
  [{:keys [wrap-throwing?] :as _opts} all-handlers]
  (reduce (fn [m k]
            (update m k (partial wrap-command-middleware
                                 wrap-throwing?)))
          all-handlers
          (keys all-handlers)))

;; ----------------------------------------------------------------------------------------

(def ^:dynamic *enable-try-catch-support* true)

"TODO: When 'throwing?' -- we only care about:
 ** unbind-name
 ** cleanup-try
 
 Alternate between processing the two so that intermediate (finally)
 forms and catch/re-throws are scoped correctly as you work your way
 back up the fn call stack."

(defn process-set-context
  [ctx]
  (let [props (:props (first (:commands ctx)))
        ctx*  (apply assoc ctx (apply concat (seq props)))]
    (next-command ctx*)))

(defn process-test-hook
  [ctx]
  (let [f (:hook-fn (first (:commands ctx)))]
    (f ctx)))

(defn create-command-handlers
  []
  (inject-command-middleware
   {:wrap-throwing? *enable-try-catch-support*}
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
    :cleanup-try     process-cleanup-try
    ;; --------------------------------------------------------------------------
    :intern-var      process-no-op ;; TODO: fix
    :recur-target    process-no-op ;; TODO: fix
    ;; --------------------------------------------------------------------------
    :set-context     process-set-context
    :test-hook       process-test-hook
    :not-implemented process-scalar}))

(comment
  "Example use case for test hook: "
  (let [f (fn [{:keys [fn-idx] :as ctx}]
            (-> ctx
                next-command
                (assoc-in [:fn-stack fn-idx :is-throwing?] true)))
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
   :is-throwing? false
   :is-finally? false
   :enable-try-catch-support? *enable-try-catch-support*})

(defn execute
  [commands]
  (let [command-handlers (create-command-handlers)]
    (loop [{:keys [enable-try-catch-support? is-throwing? fn-idx] :as ctx} (new-exec-ctx commands)]
      (let [{:keys [commands call-stack] :as _fctx} (get-in ctx [:fn-stack fn-idx])
            h (get command-handlers (:cmd (first commands)))]
        (if h
          (recur (h ctx))
          (if (and enable-try-catch-support? is-throwing?)
            ;; todo: when you support drilling down into function calls, fix
            ;; this to pop fns off the fn-stack and only throw when none remain
            ;; ---
            ;; also consider wrapping exceptions we know should be thrown inside
            ;; ex-info's with attached metadata, then wrap the entire interpreter
            ;; process with something like wrap-uncaught-ex but forward any wrapped
            ;; exception accordingly since they don't count as 'interpreter errors'
            ;; (maybe make a deftype for internal interpreter errors/exceptions)
            (throw (stack-peek call-stack))
            (stack-peek call-stack)))))))

(defn get-fctx
  [context]
  (get-in context [:fn-stack (:fn-idx context)]))

(defn execute-seq
  [handlers context]
  (cons context
        (lazy-seq
         (let [fctx (get-fctx context)
               h (get handlers (:cmd (first (:commands fctx))))]
           (when h
             (execute-seq handlers (h context)))))))

(defn execute-lazy
  [commands]
  (let [command-handlers (create-command-handlers)
        initial-ctx (new-exec-ctx commands)
        seq* (execute-seq command-handlers initial-ctx)
        final-ctx (last seq*)
        {:keys [enable-try-catch-support? is-throwing?]} final-ctx
        call-stack (:call-stack (get-fctx final-ctx))]
    (if (and enable-try-catch-support? is-throwing?)
            ;; SEE ABOVE TODO ITEM
      (throw (stack-peek call-stack))
      (stack-peek call-stack))
    seq*))

(defn ctx-seq
  [input]
  (let [command-handlers (create-command-handlers)]
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
      (throw (IllegalArgumentException. "Input must be seq or map")))))

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
          (map (fn [{:keys [fn-idx is-throwing?] :as ctx}]
                 (-> (get-in ctx [:fn-stack fn-idx])
                     (assoc :is-throwing? is-throwing?))))
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
