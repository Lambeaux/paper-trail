;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.classpath
  (:require [clojure.tools.reader :as rdr]
            [clojure.java.classpath :as jc]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [edamame.core :as ed])
  (:import [clojure.lang Namespace]
           [java.io File]
           [java.util.jar JarFile JarEntry JarInputStream]))

;; ------------------------------------------------------------------------------------------------
;; Classpath: Helpers
;; ------------------------------------------------------------------------------------------------

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

;; ------------------------------------------------------------------------------------------------
;; Classpath: Platform Agnostic Separator Regex
;; ------------------------------------------------------------------------------------------------

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

;; ------------------------------------------------------------------------------------------------
;; Classpath: Form Parsing
;; ------------------------------------------------------------------------------------------------

(def ^:dynamic *default-edamame-config*
  {:deref           true
   :quote           true
   :fn              true
   :read-eval       false
   :regex           true
   :var             true
   :auto-resolve-ns true
   ;; todo: keeping around as a reminder, will remove after more testing
   #_#_:syntax-quote    true})

(defn resolve-symbol
  [ns-override sym]
  (assert (instance? Namespace ns-override) "ns-override must be a namespace")
  (binding [*ns* ns-override]
    (rdr/resolve-symbol sym)))

(defn read-forms
  ([file-or-entry readable]
   (read-forms nil file-or-entry readable))
  ([artifact file-or-entry readable]
   (with-open [rdr  (io/reader readable)
               rdr* (ed/reader rdr)]
     (let [ns-form (ed/parse-next rdr* (ed/normalize-opts *default-edamame-config*))
           {ns-name* :current ns-aliases* :aliases} (ed/parse-ns-form ns-form)
           ;; todo: keeping around as a reminder, will remove after more testing
           #_#_opts (ed/normalize-opts *default-edamame-config*)
           opts (ed/normalize-opts
                 (assoc *default-edamame-config*
                        :syntax-quote
                        {:resolve-symbol (partial resolve-symbol (the-ns ns-name*))}))]
       {:file       file-or-entry
        :artifact   artifact
        :ns-name    ns-name*
        :ns-aliases ns-aliases*
        :ns-form    ns-form
        :forms      (->> (repeatedly (partial ed/parse-next rdr* opts))
                         (take-while #(not= % :edamame.core/eof))
                         (map #(vary-meta % assoc
                                          :file-path (str file-or-entry)
                                          :artifact-path (when artifact (str artifact))))
                         (into []))}))))

;; ------------------------------------------------------------------------------------------------
;; Classpath: Source Loading
;; ------------------------------------------------------------------------------------------------

(def extensions-clojure  #{:clj :cljc})
(def extensions-artifact #{:jar})

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

;; ------------------------------------------------------------------------------------------------
;; Classpath: Namespace Sequence
;; ------------------------------------------------------------------------------------------------

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

;; todo: add another seq fn for just discovering namespaces, but not actually parsing them
;;   (useful for cheaply building a namespace index for lazy loading source 1 fn at a time)
;; todo: add opts so you can filter out expensive tasks, like reading from jar files
(defn namespace-seq
  "Expands a classpath into a seq of maps that contain all the forms in each
   discovered namespace. Defaults to clojure.java.classpath's calculated classpath.
   Expects classpath-seq to be a seq of File objects."
  ([]
   (namespace-seq (jc/classpath)))
  ([[file & more :as _classpath-seq]]
   (let [file-type (classify-obj file)
         lazy-rest (lazy-seq (namespace-seq more))]
     ;; (println (format "Expanding [%s] %s" file-type (str file)))
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
