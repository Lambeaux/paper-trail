(ns lambeaux.paper-trail.impl.executor.reporter
  (:require [lambeaux.paper-trail.impl.executor.call-stack :as stack]
            [lambeaux.paper-trail :as-alias pt]))

(def form-event? #{:invoke-fn :end-form})

(defn ctx->fctx
  [{:keys [fn-idx] :as ctx}]
  (get-in ctx [:fn-stack fn-idx]))

(defn the-stack
  [{:keys [fn-idx] :as ctx}]
  (get-in ctx [:fn-stack fn-idx :call-stack]))

;; todo: update reporting to aggregate 
(defn wrap-reporting
  [handler]
  (fn [ctx]
    (let [{:keys [reports] :as ctx*} (handler ctx)
          {:keys [commands] {::pt/keys [var-id src-meta]} :fn-meta} (ctx->fctx ctx)
          {:keys [action type op form-meta]} (first commands)]
      ;; (println "Action: " action)
      ;; (println "Type: " type)
      (if-not (form-event? action)
        ctx*
        (case action
          :invoke-fn (let [[f & args :as _frame] (stack/peek-frame (the-stack ctx))
                           op* (or op (symbol (pr-str f)))
                           report {:fn-id var-id
                                   :fn-meta src-meta
                                   :form (apply list op* args)
                                   :form-meta nil
                                   :result (stack/peek-val (the-stack ctx*))}]
                       ;; (println "Report: " (pr-str report))
                       (update ctx* :reports conj report))
          :end-form  (let [idx (dec (count reports))]
                       ;; (println "Form Meta: " (pr-str form-meta))
                       ;; (println "Report Idx: " idx)
                       (if (or (not= :fn type) 
                               (< idx 0))
                         ctx*
                         (update-in ctx* [:reports idx] assoc :form-meta form-meta))))))))
