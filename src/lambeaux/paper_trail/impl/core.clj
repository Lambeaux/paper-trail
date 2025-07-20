;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.core
  (:require [lambeaux.paper-trail.impl.generator :as ptg]
            [lambeaux.paper-trail.impl.executor :as pte])
  (:import [clojure.lang IObj]))

;; ----------------------------------------------------------------------------------------

(defn evaluate
  ([form]
   (evaluate form nil))
  ([form args]
   (let [cmds (if args
                (ptg/create-commands form args)
                (ptg/create-commands form))]
     (pte/execute cmds))))

(defn evaluate-to
  ([idx form]
   (evaluate-to idx form nil))
  ([idx form args]
   (let [cmds (if args
                (ptg/create-commands form args)
                (ptg/create-commands form))]
     (pte/execute cmds idx))))

(defn evaluate-debug
  ([form]
   (evaluate-debug form nil))
  ([form args]
   (evaluate-debug form args identity))
  ([form args xform]
   (let [cmds (if args
                (ptg/create-commands form args)
                (ptg/create-commands form))]
     (->> cmds
          (pte/ctx-seq)
          (map (fn [{:keys [fn-idx throwing-ex is-throwing? is-finally?] :as ctx}]
                 (-> (get-in ctx [:fn-stack fn-idx])
                     (assoc
                      :is-throwing? is-throwing?
                      :is-finally? is-finally?
                      :throwing-ex (boolean throwing-ex)))))
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

;; ----------------------------------------------------------------------------------------

(comment 
  (evaluate '(+ 1 1))
  (evaluate-to 0 '(+ 1 1))
  (evaluate-debug '(+ 1 1)))
