;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.wip.classpath
  (:require [clojure.java.classpath :as jc]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.walk :as w]
            [edamame.core :as ed])
  (:import [java.io File]
           [java.util.jar JarFile JarEntry JarInputStream]))

;; ----------------------------------------------------------------------------------------
;; CLASSPATH SOURCE LOADING
;; ----------------------------------------------------------------------------------------

(def class->kind {File :file JarEntry :entry})
(def supported-clojure-extensions  #{:clj :cljc})
(def supported-artifact-extensions #{:jar})

(def all-extensions (set/union supported-clojure-extensions
                               supported-artifact-extensions))

(declare obj->map-fn)

(defn children-fn
  [{:keys [obj kind ext dir?] :as m} _parent]
  (case [kind ext]
    [:file nil] (fn []
                  (when dir?
                    (mapv (obj->map-fn m) (.listFiles obj))))
    [:file :jar] (fn []
                   (with-open [jar-stream (JarInputStream. (io/input-stream obj))]
                     (into []
                           (comp (take-while identity)
                                 (map (obj->map-fn m)))
                           (repeatedly (fn [] (.getNextJarEntry jar-stream))))))
    (constantly [])))

(defn content-fn
  [{:keys [obj kind] :as _m} parent]
  (case kind
    :file  (fn [] (slurp obj))
    :entry (fn [] (let [jar-file (JarFile. (:obj parent))]
                    (slurp (.getInputStream jar-file obj))))
    (constantly nil)))

(defn obj->map-fn
  [parent]
  (fn [obj]
    (let [kind (class->kind (class obj))
          dir? (.isDirectory obj)
          ext  (-> (.getName obj)
                   (str/split #"\.")
                   last
                   keyword
                   all-extensions)
          m    {:obj obj :kind kind :dir? dir? :ext ext}]
      (assoc m
             :children (children-fn m parent)
             :content  (content-fn  m parent)))))

(defn files->source-cache
  [files]
  (map (obj->map-fn nil) files))

(comment

  (def my-test-map
    (->> (jc/classpath)
         files->source-cache
         (map #(vector (.getName (:obj %)) %))
         (into {})))

  (mapcat (fn [{:keys [children]}]
            (children))
          [(get my-test-map "edamame-1.4.27.jar")])

  (binding [*out* (java.io.PrintWriter. System/out)
            *err* (java.io.PrintWriter. System/err)]
    (println "Hi there")))

;; ----------------------------------------------------------------------------------------
;; EDAMAME EXPERIMENTS
;; ----------------------------------------------------------------------------------------

(defn make-opts
  "Create Edamame options."
  ([]
   (make-opts true))
  ([fn-handler]
   (assert (or (boolean? fn-handler) (fn? fn-handler))
           "fn-handler must be boolean or fn")
   {:deref true
    :quote true
    :fn fn-handler
    :read-eval false
    :regex true
    :var true}))

(defn parse
  "Parse Clojure source using Edamame."
  ([text]
   (parse :default text))
  ([mode-or-fn text]
   (case mode-or-fn
     :default  (ed/parse-string text (make-opts))
     :identity (ed/parse-string text (make-opts identity))
     (ed/parse-string text (make-opts mode-or-fn)))))

(defn pt-arg-namegen
  "This is one potential strategy Paper Trail can use for generating argument
   names. Since symbols support metadata, there's a lot of flexibility here.
   The UUID pieces might not be necessary. An atomic counter here in static
   memory might suffice."
  [arg-names sym]
  (let [randomness (apply str (take 8 (str (random-uuid))))
        arg-num (or (seq (rest (name sym))) [1])
        name-parts (concat ["pt_arg_"] arg-num ["_" randomness])
        new-name (vary-meta (symbol (apply str name-parts))
                            assoc
                            :paper.trail/generated-name? true
                            :paper.trail/arg-idx (parse-long (apply str arg-num)))]
    (swap! arg-names conj new-name)
    new-name))

(defn pt-arg-rename
  "Simple mapper for use with clojure.walk for renaming fn arg symbols in a
   given input form."
  [arg-names form]
  (if-not (and (simple-symbol? form)
               (str/starts-with? (name form) "%"))
    form
    (pt-arg-namegen arg-names form)))

(defn pt-fn-reader
  "This is one potential strategy Paper Trail can use for transforming functions
   defined using the shorthand reader notation."
  [form]
  (let [arg-names (atom #{})
        body (w/prewalk (partial pt-arg-rename arg-names) form)
        arglist (into [] (sort @arg-names))]
    (list 'fn* arglist body)))

(comment

  "EDAMAME EXAMPLES"

  "Never would have thought this was valid, but it is."
  (let [%8 10
        f (fn [%1] (inc %1))]
    (f %8))

  "Might prefer this instead. More readable ?? Possibly clashes with existing names ??"
  (let [a8 10
        f (fn [a1] (inc a1))]
    (f a8))

  (parse :default "#(inc %)")
  (parse :default "(map #(inc %) [1 2 3])")

  (parse :identity "#(inc %)")
  (parse :identity "(map #(inc %) [1 2 3])")

  (parse pt-fn-reader "#(inc %)")
  (parse pt-fn-reader "(map #(inc %) [1 2 3])")
  (parse pt-fn-reader "#(* %1 %2 %3)"))

(comment

  "Checking our work"

  (defn verify-meta
    [form]
    (let [state (atom [])
          walker (fn [form*]
                   (if (simple-symbol? form*)
                     (swap! state conj [form* (meta form*)])
                     (swap! state conj [(type form*) (meta form*)]))
                   form*)]
      (w/prewalk walker form)
      (deref state)))

  ;; watch out, you're losing edamame metadata with your custom 
  ;; reader fn ... [fn* nil]
  (verify-meta (parse pt-fn-reader "#(* %1 %2 %3)")))
