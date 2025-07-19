(ns lambeaux.paper-trail.misc.impl-v2-btsi
  (:require [lambeaux.paper-trail.impl.util :as ptu]
            [lambeaux.paper-trail :as-alias pt])
  (:import [clojure.lang IObj]))

(def temp-ctx {::pt/current-ns *ns*})

;; ---------------------------------------------------------
;; Bidirectional Tree Seq Interpreter
;; ---------------------------------------------------------

(defn child-seq [x]
  (let [has-children? #(if (string? %) false (seqable? %))]
    (tree-seq has-children? seq x)))

(declare post-process)

(defn handle-vector
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (tap> ["Handle Vector" ctx])
  (let [exp-size (count exp)
        result (into [] (take exp-size args))]
    (lazy-seq
     (post-process (assoc ctx
                          :forms exprs
                          :args (conj (drop exp-size args) result))))))

(defn handle-map
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (tap> ["Handle Map" ctx])
  (let [exp-size (count exp)
        result (into {} (take exp-size args))]
    (lazy-seq
     (post-process (assoc ctx
                          :forms exprs
                          :args (conj (drop exp-size args) result))))))

(defn handle-list-fn
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (tap> ["Handle List Fn" ctx])
  (let [exp-size (count exp)
        result (apply (ptu/->impl temp-ctx (first exp))
                      (map (ptu/map-impl-fn temp-ctx)
                           (rest (take exp-size args))))
        result (if (instance? IObj result)
                 (with-meta result {::pt/is-evaled? true})
                 result)]
    (cons {:form (map (fn [arg]
                        (or (and (instance? IObj arg)
                                 (::pt/raw-form (meta arg)))
                            arg))
                      (take exp-size args))
           :result result}
          (lazy-seq
           (post-process (assoc ctx
                                :forms (cons result exprs)
                                :args (drop exp-size args)))))))

(defn handle-fn-literal
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (lazy-seq
   (post-process
    (assoc ctx
           :forms exprs
           :args (conj args (with-meta (eval exp) {::pt/raw-form exp}))))))

(defn prepare-fn-literal
  [{:keys [forms] [exp & exprs] :input :as ctx}]
  (tap> ["Prepare Fn Literal" ctx])
  (assoc ctx
         :input (drop (dec (count (child-seq exp))) exprs)
         :forms (conj forms exp)))

(defn prepare-macro
  [{:keys [forms] [exp & _exprs] :input :as ctx}]
  (tap> ["Prepare Macro" ctx])
  ;; Reminder: Calling macroexpand converts all lists to be Cons (nested)
  (let [expanded (macroexpand exp)]
    (assoc ctx
           :input (child-seq expanded)
           :forms forms)))

(def dispatch-pre {'fn              prepare-fn-literal
                   'fn*             prepare-fn-literal
                   :type/list-macro prepare-macro})

(def dispatch-post {'fn             handle-fn-literal
                    'fn*            handle-fn-literal
                    :type/list-fn   handle-list-fn
                    :type/vector    handle-vector
                    :type/map       handle-map})

(defn pre-process
  [ctx]
  (loop [{:keys [dispatch-pre forms] [exp & exprs] :input :as ctx*} ctx]
    (let [type-kw (ptu/classify exp)
          handler (or (when (list? exp) (dispatch-pre (first exp)))
                      (dispatch-pre type-kw))]
      (tap> ["Pre Process" type-kw handler ctx*])
      (cond
        (nil? exp)
        ctx*
        (fn? handler)
        ;; Possibly omit recur and just do a lazy-seq in handlers
        (recur (handler ctx*))
        :else
        (recur (assoc ctx*
                      :input exprs
                      :forms (conj forms exp)))))))

(defn post-process
  [ctx]
  (loop [{:keys [dispatch-post args scope] [exp & exprs] :forms :as ctx*} ctx]
    (let [type-kw (ptu/classify exp)
          handler (or (when (list? exp) (dispatch-post (first exp)))
                      (dispatch-post type-kw))
          {::pt/keys [is-evaled?]} (meta exp)]
      (tap> ["Post Process" type-kw handler ctx*])
      (cond
        (nil? exp)
        nil
        is-evaled?
        (recur (assoc ctx* :forms exprs :args (conj args exp)))
        (fn? handler)
        (handler ctx*)
        (and (symbol? exp)
             (contains? scope exp))
        (recur (assoc ctx*
                      :forms exprs
                      :args (conj args (scope exp))))
        :else
        (recur (assoc ctx* :forms exprs :args (conj args exp)))))))

(defn trace-seq*
  ([ctx]
   (post-process (pre-process ctx)))
  ([form scope]
   (trace-seq* {:dispatch-pre dispatch-pre
                :dispatch-post dispatch-post
                :input (child-seq form)
                :forms (list)
                :args (list)
                :scope scope})))

(defn trace-seq
  [form scope]
  (trace-seq* form scope))
