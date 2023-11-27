(ns lambeaux.paper-trail.seq
  (:require [lambeaux.paper-trail.core :as core]
            [paper.trail :as-alias pt]))

(def temp-ctx {::pt/current-ns *ns*})

(defn child-seq [x]
  (let [has-children? #(if (string? %) false (seqable? %))]
    (tree-seq has-children? seq x)))

;; - Use symbols to represent literals in a dispatch map ('let, 'try, etc)
;; - Use keywords to represent types in a dispatch map (:list, :vector, :map, etc)
;; - Use multi-methods to arrive at the key when preds are needed??
;; - Use a map to arrive at dispatch keywords??

(defn classify [x]
  (cond (string? x)            :type/string
        (number? x)            :type/number
        (boolean? x)           :type/boolean
        (char? x)              :type/char
        (class? x)             :type/class
        (fn? x)                :type/fn
        (nil? x)               :type/nil
        (simple-keyword? x)    :type/keyword-simple
        (simple-symbol? x)     :type/symbol-simple
        (qualified-keyword? x) :type/keyword-qual
        (qualified-symbol? x)  :type/symbol-qual
        (map? x)               :type/map
        (vector? x)            :type/vector
        (set? x)               :type/set
        (list? x)              :type/list
        (seq? x)               :type/seq
        :else (throw (ex-info (str "Type " (type x) " is unknown for value: " x)
                              {:value x :type (type x)}))))

(declare post-process)

(defn handle-vector
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (let [exp-size (count exp)
        result (into [] (take exp-size args))]
    (lazy-seq
     (post-process (assoc ctx
                          :forms exprs
                          :args (conj (drop exp-size args) result))))))

(defn handle-map
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (let [exp-size (count exp)
        result (into {} (take exp-size args))]
    (lazy-seq
     (post-process (assoc ctx
                          :forms exprs
                          :args (conj (drop exp-size args) result))))))

(defn handle-list
  [{:keys [args] [exp & exprs] :forms :as ctx}]
  (let [exp-size (count exp)
        result (apply (core/->impl temp-ctx (first exp))
                      (map (core/map-impl-fn temp-ctx)
                           (rest (take exp-size args))))
        result (if (instance? clojure.lang.IObj result)
                 (with-meta result {::pt/is-evaled? true})
                 result)]
    (cons {:form (take exp-size args) :result result}
          (lazy-seq
           (post-process (assoc ctx
                                :forms (cons result exprs)
                                :args (drop exp-size args)))))))

(defn prepare-fn-literal
  [{:keys [forms] [exp & exprs] :input :as ctx}]
  (assoc ctx
         :input (drop (dec (count (child-seq exp))) exprs)
         :forms (conj forms (eval exp))))

(def dispatch-pre {'fn* prepare-fn-literal})

(def dispatch-post {:type/list   handle-list
                    :type/vector handle-vector
                    :type/map    handle-map})

(defn pre-process
  [ctx]
  (loop [{:keys [dispatch-pre forms] [exp & exprs] :input :as ctx*} ctx]
    (let [type-kw (classify exp)
          handler (or (when (= type-kw :type/list) (dispatch-pre (first exp)))
                      (dispatch-pre type-kw))]
      (tap> ["Pre Process: " ctx*])
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
    (let [handler (dispatch-post (classify exp))
          {::pt/keys [is-evaled?]} (meta exp)]
      (tap> ["Post Process: " ctx*])
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
