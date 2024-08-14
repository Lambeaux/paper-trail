(ns lambeaux.paper-trail.decomp
  (:require [lambeaux.paper-trail.core :as core]
            [lambeaux.paper-trail.seq :as pts]
            [paper.trail :as-alias pt])
  (:import [clojure.lang IObj Cons]))

(defn decompose-form [form]
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
            [{:cmd :capture-state :id id}
             {:cmd :exec-when :id id :count (count cond-pos)}]
            cond-pos
            (when cond-neg
              [{:cmd :skip-when :id id :count (count cond-neg)}])
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
                              [{:cmd :bind-name :id bname :impl? in-macro-impl?}])) 
                    bindings)
            (mapcat form->commands body)
            [{:cmd :end-form :type :special :op (first form) :impl? in-macro-impl?}])))

(defn handle-macro
  [{:keys [form->commands in-macro-impl?] :as attrs} form]
  (let [form* (cons (first form)
                    (map #(vary-meta % assoc ::pt/macro-arg true)
                         (rest form)))]
    (concat [{:cmd :begin-form :type :macro :op (first form) :impl? in-macro-impl?}]
            (form->commands (assoc attrs :in-macro-impl? true)
                            (macroexpand form*))
            [{:cmd :end-form :type :macro :op (first form) :impl? in-macro-impl?}])))

(def form-handlers {'do              (wrap-check-macro handle-do)
                    'if              (wrap-check-macro handle-if)
                    'let             (wrap-check-macro handle-let)
                    'let*            (wrap-check-macro handle-let)
                    :type/list-fn    (wrap-check-macro handle-invoke)
                    :type/list-macro (wrap-check-macro handle-macro)})

(defn form->commands
  ([form]
   (form->commands {:form->commands form->commands :in-macro-impl? false} form))
  ([attrs form]
   (when form
     (if-not (coll? form)
       (seq [{:cmd :scalar :form form}])
       (let [h (or (get form-handlers (first form))
                   (get form-handlers (pts/classify form)))]
         (if h
           (lazy-seq (h attrs form))
           (seq [{:cmd :not-implemented :form form}])))))))

;; ----------------------------------------------------------------------------------------

(def temp-ctx {::pt/current-ns *ns*})

(defn process-no-op
  [{[_cmd & cmds] :commands :as ctx}]
  (assoc ctx :commands cmds))

(defn process-invoke
  [{:keys [args] [cmd & cmds] :commands :as ctx}]
  (let [arg-count (:arg-count cmd)
        result (apply (core/->impl temp-ctx (:op cmd))
                      (map (core/map-impl-fn temp-ctx)
                           (take arg-count args)))
        result (if (instance? IObj result)
                 (with-meta result {::pt/is-evaled? true})
                 result)
        _final-form (map (fn [arg]
                           (or (and (instance? IObj arg)
                                    (::pt/raw-form (meta arg)))
                               arg))
                         (take arg-count args))]
    (assoc ctx :commands cmds :args (conj (into (list) (drop arg-count args)) result))))

(defn process-scalar
  [{:keys [args source-scope impl-scope] [cmd & cmds] :commands :as ctx}]
  (let [scalar-form (:form cmd)
        scalar-value (or (peek (get source-scope scalar-form))
                         (peek (get impl-scope scalar-form))
                         scalar-form)]
    (assoc ctx :commands cmds :args (conj args scalar-value))))

(defn process-capture-state
  [{:keys [args state] [cmd & cmds] :commands :as ctx}]
  (assoc ctx :commands cmds :state (assoc state (:id cmd) (peek args))))

(defn process-bind-name
  [{:keys [args source-scope impl-scope] [{:keys [id impl?]} & cmds] :commands :as ctx}]
  (if impl?
    (assoc ctx :commands cmds :impl-scope (update impl-scope id #(conj % (peek args))))
    (assoc ctx :commands cmds :source-scope (update source-scope id #(conj % (peek args))))))

(defn process-unbind-name
  [{:keys [source-scope impl-scope] [{:keys [id impl?]} & cmds] :commands :as ctx}]
  (if impl?
    (assoc ctx :commands cmds :impl-scope (update impl-scope id pop))
    (assoc ctx :commands cmds :source-scope (update source-scope id pop))))

(defn process-exec-when
  [{:keys [state] [cmd & cmds] :commands :as ctx}]
  (if (get state (:id cmd))
    (assoc ctx :commands cmds)
    (assoc ctx :commands (drop (:count cmd) cmds))))

(defn process-skip-when
  [{:keys [state] [cmd & cmds] :commands :as ctx}]
  (if (get state (:id cmd))
    (assoc ctx :commands (drop (:count cmd) cmds))
    (assoc ctx :commands cmds)))

(def command-handlers {:begin-form process-no-op
                       :end-form process-no-op
                       :invoke-fn process-invoke
                       :scalar process-scalar
                       :capture-state process-capture-state
                       :bind-name process-bind-name
                       :unbind-name process-unbind-name
                       :exec-when process-exec-when
                       :skip-when process-skip-when
                       :not-implemented process-scalar})

(defn execute [commands]
  (loop [{:keys [commands args] :as ctx} {:commands commands 
                                          :args (list) 
                                          :source-scope {}
                                          :impl-scope {}
                                          :state {}}]
    (let [h (get command-handlers (:cmd (first commands)))]
      (if h
        (recur (h ctx))
        (first args)))))
