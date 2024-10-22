(ns lambeaux.paper-trail.decomp
  (:require [lambeaux.paper-trail.core :as core]
            [lambeaux.paper-trail.seq :as pts]
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

(def form-handlers
  {'def             (wrap-check-macro handle-def)
   'do              (wrap-check-macro handle-do)
   'if              (wrap-check-macro handle-if)
   'let             (wrap-check-macro handle-let)
   'let*            (wrap-check-macro handle-let)
   'loop            (wrap-check-macro handle-loop)
   'recur           (wrap-check-macro handle-recur)
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

(def temp-ctx {::pt/current-ns *ns*})

(defn next-command
  [{:keys [command-history] [cmd & cmds] :commands :as fctx}]
  (assoc fctx :commands cmds :command-history (conj command-history cmd)))

(defn process-no-op
  [{:keys [fn-idx] :as ctx}]
  (update-in ctx [:fn-stack fn-idx] next-command))

(defn process-invoke
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [call-stack] [cmd & _] :commands} fctx
        arg-count (:arg-count cmd)
        result (apply (core/->impl temp-ctx (:op cmd))
                      (map (core/map-impl-fn temp-ctx)
                           (take arg-count call-stack)))
        result (if (instance? IObj result)
                 (with-meta result {::pt/is-evaled? true})
                 result)
        _final-form (map (fn [arg]
                           (or (and (instance? IObj arg)
                                    (::pt/raw-form (meta arg)))
                               arg))
                         (take arg-count call-stack))]
    (update-in ctx [:fn-stack fn-idx]
               #(-> %
                    next-command
                    (assoc :call-stack
                           (conj (->> (drop arg-count call-stack)
                                      (reverse)
                                      (into (list)))
                                 result))))))

(defn process-scalar
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [call-stack source-scope impl-scope] [cmd & _] :commands} fctx
        scalar-form (:form cmd)
        scalar-value (or (peek (get source-scope scalar-form))
                         (peek (get impl-scope scalar-form))
                         scalar-form)
        scalar-value (case scalar-value
                       ::pt/nil nil
                       ::pt/false false
                       scalar-value)]
    (update-in ctx [:fn-stack fn-idx]
               #(-> %
                    next-command
                    (assoc :call-stack (conj call-stack scalar-value))))))

(defn process-capture-state
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [call-stack state] [cmd & _] :commands} fctx]
    (update-in ctx [:fn-stack fn-idx]
               #(-> %
                    next-command
                    (assoc :state (assoc state (:state-id cmd) (peek call-stack)))))))

(defn process-bind-name
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [state call-stack source-scope impl-scope]
         [{:keys [bind-id bind-from impl? value state-id]} & _] :commands} fctx
        val-to-bind (case bind-from
                      :command-value value
                      :call-stack (peek call-stack)
                      :state (get state state-id))
        val-to-bind (case val-to-bind
                      nil ::pt/nil
                      false ::pt/false
                      val-to-bind)]
    (update-in ctx [:fn-stack fn-idx]
               (fn [m]
                 (next-command
                  (if impl?
                    (assoc m :impl-scope
                           (update impl-scope bind-id #(conj % val-to-bind)))
                    (assoc m :source-scope
                           (update source-scope bind-id #(conj % val-to-bind)))))))))

(defn process-unbind-name
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [source-scope impl-scope] [{:keys [bind-id impl?]} & _] :commands} fctx]
    (update-in ctx [:fn-stack fn-idx]
               #(next-command
                 (if impl?
                   (assoc % :impl-scope (update impl-scope bind-id pop))
                   (assoc % :source-scope (update source-scope bind-id pop)))))))

(defn process-exec-when
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [state command-history] [cmd & cmds] :commands} fctx]
    (update-in ctx [:fn-stack fn-idx]
               #(if (get state (:state-id cmd))
                  (assoc %
                         :commands cmds
                         :command-history (conj command-history cmd))
                  (assoc %
                         :commands (drop (:count cmd) cmds)
                         :command-history (into (conj command-history cmd)
                                                (take (:count cmd) cmds)))))))

(defn process-skip-when
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [state command-history] [cmd & cmds] :commands} fctx]
    (update-in ctx [:fn-stack fn-idx]
               #(if (get state (:state-id cmd))
                  (assoc %
                         :commands (drop (:count cmd) cmds)
                         :command-history (into (conj command-history cmd)
                                                (take (:count cmd) cmds)))
                  (assoc %
                         :commands cmds
                         :command-history (conj command-history cmd))))))

(defn process-replay-commands
  [{:keys [fn-idx] :as ctx}]
  (let [fctx (get-in ctx [:fn-stack fn-idx])
        {:keys [command-history] [{:keys [idx] :as cmd} & cmds] :commands} fctx
        cmds-to-replay (take-while (fn [old-cmd]
                                     (not (and (= :recur-target (:cmd old-cmd))
                                               (= idx (:idx old-cmd)))))
                                   command-history)]
    (update-in ctx [:fn-stack fn-idx]
               #(assoc %
                       :commands (concat (reverse cmds-to-replay) [cmd] cmds)
                       :command-history (drop (count cmds-to-replay) command-history)))))

(def command-handlers
  {:begin-form process-no-op
   :end-form process-no-op
   :invoke-fn process-invoke
   :scalar process-scalar
   :capture-state process-capture-state
   :bind-name process-bind-name
   :unbind-name process-unbind-name
   :exec-when process-exec-when
   :skip-when process-skip-when
   :replay-commands process-replay-commands
   :intern-var process-no-op ;; fix
   :recur-target process-no-op ;; fix
   :not-implemented process-scalar})

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
   :throwing? false})

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
