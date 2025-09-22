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
;; MISC
;; ----------------------------------------------------------------------------------------

(defn new-jar-input-stream
  [stream-in]
  (JarInputStream. stream-in))

(defn is-file?
  [obj]
  (instance? File obj))

(defn is-jar-file?
  [obj]
  (instance? JarFile obj))

(defn is-jar-entry?
  [obj]
  (instance? JarEntry obj))

;; ----------------------------------------------------------------------------------------
;; PLATFORM AGNOSTIC SEPARATOR REGEX
;; ----------------------------------------------------------------------------------------

(defn path-regex
  "Build a platform agnostic path regex using sep as the separator string."
  [sep]
  (let [sep-char (first sep)
        sep-str  (or (char-escape-string sep-char)
                     (str sep-char))]
    (re-pattern (str "\\.|" sep-str))))

(def path-os  (path-regex File/separator))
(def path-jar (path-regex "/"))

(defn path-filter
  "Input 'obj' can be anything where (str obj) returns a path string.
   Should work for java.io.File and java.util.jar.JarEntry (and probably others)."
  [path-regex pred]
  (fn [obj]
    (pred (str/split (str obj) path-regex))))

;; ----------------------------------------------------------------------------------------
;; PREDS
;; ----------------------------------------------------------------------------------------

(def extensions-clojure  #{:clj :cljc})
(def extensions-artifact #{:jar})
(def extensions-all (set/union extensions-clojure extensions-artifact))

(defn only-clojure-exts?
  [path-parts]
  (contains? extensions-clojure
             (keyword (last path-parts))))

(defn only-artifact-exts?
  [path-parts]
  (contains? extensions-artifact
             (keyword (last path-parts))))

(defn only-no-metainf
  [path-parts]
  (not= "META-INF" (first path-parts)))

;; ----------------------------------------------------------------------------------------
;; CLASSPATH FORM PARSING
;; ----------------------------------------------------------------------------------------

(comment
  (fn [file*]
    (contains? extensions-clojure
               (keyword (last (str/split (str file*) #"\."))))))

(def class->kind {File :file JarEntry :entry})

(def ^:dynamic *default-edamame-config*
  {:deref           true
   :quote           true
   :fn              true
   :read-eval       false
   :regex           true
   :var             true
   :auto-resolve-ns true})

(defn read-forms
  ([file-or-entry readable]
   (read-forms nil file-or-entry readable))
  ([artifact file-or-entry readable]
   (let [opts (ed/normalize-opts *default-edamame-config*)]
     (with-open [rdr  (io/reader readable)
                 rdr* (ed/reader rdr)]
       {:file     file-or-entry
        :artifact artifact
        :forms    (->> (repeatedly (partial ed/parse-next rdr* opts))
                       (take-while #(not= % :edamame.core/eof))
                       (map #(vary-meta % assoc
                                        :file-path (str file-or-entry)
                                        :artifact-path (when artifact (str artifact))))
                       (into []))}))))

;; ----------------------------------------------------------------------------------------
;; CLASSPATH SOURCE LOADING
;; ----------------------------------------------------------------------------------------

(defn read-raw-file
  ([obj-in]
   (read-raw-file nil obj-in))
  ([_parent obj-in]
   (assert (is-file? obj-in) "obj-in must be a file")
   (read-forms obj-in obj-in)))

(defn read-raw-directory
  ([obj-in]
   (read-raw-directory nil obj-in))
  ([_parent obj-in]
   (assert (is-file? obj-in) "obj-in must be a file")
   (assert (.isDirectory obj-in) "obj-in must be a directory")
   (filter (path-filter path-os only-clojure-exts?)
           (file-seq obj-in))))

(defn read-jar-entry
  ([obj-in]
   (throw (UnsupportedOperationException.
           (str "JarEntry requires the parent JarFile in order to perform a read: "
                obj-in))))
  ([parent-jar obj-in]
   (assert (is-jar-file? parent-jar) "parent must be a jar file")
   (assert (is-jar-entry? obj-in) "obj-in must be a jar entry")
   (with-open [stream (.getInputStream parent-jar obj-in)]
     (read-forms parent-jar obj-in stream))))

(defn read-jar-archive
  ([obj-in]
   (read-jar-archive nil obj-in))
  ([_parent obj-in]
   (assert (is-file? obj-in) "obj-in must be a file")
   (with-open [jar-stream (new-jar-input-stream (io/input-stream obj-in))]
     (->> (repeatedly (fn [] (.getNextJarEntry jar-stream)))
          (take-while identity)
          (filter (path-filter path-jar
                               (every-pred only-clojure-exts?
                                           only-no-metainf)))
          (map (partial read-jar-entry (JarFile. obj-in)))
          (into [])))))

;; ----------------------------------------------------------------------------------------
;; CLASSPATH SEQ
;; ----------------------------------------------------------------------------------------

(def is-directory?   #(when (is-file? %) (.isDirectory %)))
(def is-source-file? (path-filter path-os only-clojure-exts?))
(def is-artifact?    (path-filter path-os only-artifact-exts?))

(defn classify-obj
  [obj]
  (cond
    (and (is-file? obj)
         (is-directory? obj))    :raw-directory
    (and (is-file? obj)
         (is-source-file? obj))  :raw-file
    (and (is-file? obj)
         (is-artifact? obj))     :jar-archive
    (and (map? obj)
         (contains? obj :forms)) :parsed
    :else                        :unsupported))

(defn namespace-seq
  "Expands a classpath into a seq of maps that contain all the forms in each
   discovered namespace. Defaults to clojure.java.classpath's calculated classpath.
   Expects classpath-seq to be a seq of File objects."
  ([]
   (namespace-seq (jc/classpath)))
  ([[file & more :as _classpath-seq]]
   (let [file-type (classify-obj file)
         lazy-rest (lazy-seq (namespace-seq more))]
     (println (format "Expanding [%s] %s" file-type (str file)))
     (case file-type
       :parsed        (cons file lazy-rest)
       :raw-file      (cons (read-raw-file file) lazy-rest)
       :jar-archive   (concat (read-jar-archive file) lazy-rest)
       :raw-directory (lazy-seq (namespace-seq
                                 (concat (read-raw-directory file) more)))
       :unsupported   (do (println
                           (format "Warning: Unexpected classpath object [%s] %s"
                                   (class file)
                                   (str file)))
                          lazy-rest)))))

;; Different streamable types on the classpath
{:type      [:raw-directory :raw-file :jar-archive :jar-entry]
 :is-dir?    false
 :is-zipped? false
 :is-local?  false}

;; ----------------------------------------------------------------------------------------
;; CLASSPATH OUTDATED EXPERIMENTS
;; ----------------------------------------------------------------------------------------

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
                   extensions-all)
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
