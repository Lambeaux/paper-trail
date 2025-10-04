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
            [edamame.core :as ed]
            [lambeaux.paper-trail :as-alias pt])
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

(defn artifact->path
  [artifact]
  (if (instance? JarFile artifact)
    (.getName artifact)
    (str artifact)))

(defn path-filter
  "Input 'obj' can be anything where (str obj) returns a path string.
   Should work for java.io.File and java.util.jar.JarEntry (and probably others)."
  [path-regex pred]
  (fn [obj]
    (pred (str/split (artifact->path obj) path-regex))))

;; ------------------------------------------------------------------------------------------------
;; Classpath: Form Parsing
;; ------------------------------------------------------------------------------------------------

;; todo: fix configuration to add support for reader conditionals
;;   .m2/repository/aero/aero/1.1.6/aero-1.1.6.jar | edamame error 'Conditional read not allowed.'
(def ^:dynamic *default-edamame-config*
  {:deref           true
   :quote           true
   :fn              true
   :read-eval       false
   :regex           true
   :var             true
   :auto-resolve-ns true
   ;; note: for now, only JVM Clojure is supported, but this will evolve
   :read-cond       :allow
   :features        #{:clj}
   ;; todo: keeping around as a reminder, will remove after more testing
   #_#_:syntax-quote    true})

(def ^:dynamic *enable-parse-all-forms* false)

(defn resolve-symbol
  [ns-name* sym]
  (let [ns-override (the-ns ns-name*)]
    (assert (instance? Namespace ns-override) "ns-override must be a namespace")
    (binding [*ns* ns-override]
      (rdr/resolve-symbol sym))))

(defn classpath-record
  [map-in]
  (with-meta map-in {::pt/classpath-record? true}))

(defn location-info
  [artifact file-or-entry]
  {:path-to-ns (str file-or-entry)
   :path-to-artifact (when artifact (artifact->path artifact))
   :file (when (is-file? file-or-entry) file-or-entry)
   :jar-entry (when (is-jar-entry? file-or-entry) file-or-entry)
   :artifact artifact})

(defn read-forms
  ([file-or-entry readable]
   (read-forms nil file-or-entry readable))
  ([artifact file-or-entry readable]
   (with-open [rdr  (io/reader readable)
               rdr* (ed/reader rdr)]
     (let [ns-form (ed/parse-next rdr* (ed/normalize-opts *default-edamame-config*))
           {ns-id :current aliases :aliases} (ed/parse-ns-form ns-form)
           ;; todo: keeping around as a reminder, will remove after more testing
           #_#_opts (ed/normalize-opts *default-edamame-config*)
           opts (ed/normalize-opts
                 (assoc *default-edamame-config*
                        :syntax-quote
                        {:resolve-symbol (partial resolve-symbol ns-id)}))
           location* (location-info artifact file-or-entry)]
       (classpath-record
        (merge
         location*
         {:ns-id      ns-id
          :ns-form    ns-form
          :alias-map  aliases
          :forms      (when *enable-parse-all-forms*
                        (->> (repeatedly (partial ed/parse-next rdr* opts))
                             (take-while #(not= % :edamame.core/eof))
                             (map #(vary-meta % assoc ::pt/location location*))
                             (into [])))}))))))

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

(defn only-non-metainf?
  [path-parts]
  (not= "META-INF" (first path-parts)))

(def pred-file (path-filter path-os
                            only-clojure-exts?))

(def pred-jar  (path-filter path-jar
                            (every-pred only-clojure-exts?
                                        only-non-metainf?)))

(defn read-raw-file
  ([source-file]
   (assert (is-file? source-file) "source-file must be a file")
   (read-forms source-file source-file)))

(defn read-raw-directory
  ([directory-as-file]
   (assert (is-file? directory-as-file) "directory-as-file must be a file")
   (assert (.isDirectory directory-as-file) "directory-as-file must be a directory")
   (filter pred-file (file-seq directory-as-file))))

(defn read-jar-entry
  ([parent-file jar-entry]
   (assert (is-file? parent-file) "parent-file must be a file")
   (assert (is-jar-entry? jar-entry) "jar-entry must be a jar entry")
   (with-open [entry-stream (.getInputStream (JarFile. parent-file) jar-entry)]
     (read-forms parent-file jar-entry entry-stream))))

(defn read-jar-archive
  ([mapper-fn jar-file]
   (assert (is-file? jar-file) "jar-file must be a file")
   (with-open [jar-stream (new-jar-input-stream (io/input-stream jar-file))]
     (->> (repeatedly (fn [] (.getNextJarEntry jar-stream)))
          (take-while identity)
          (filter pred-jar)
          (map mapper-fn)
          (into [])))))

(defn read-jar-content
  ([jar-file]
   (read-jar-archive (partial read-jar-entry jar-file) jar-file)))

(defn read-jar-single-entry
  [jar-artifact jar-entry]
  (with-open [_jar-stream (new-jar-input-stream (io/input-stream jar-artifact))]
    (read-jar-entry jar-artifact jar-entry)))

;; ------------------------------------------------------------------------------------------------

(defn listing-for-file
  ([obj-in]
   (classpath-record (location-info nil obj-in))))

(defn listings-for-directory
  ([obj-in]
   (map #(classpath-record (location-info nil %))
        (read-raw-directory obj-in))))

(defn listings-for-jar
  ([obj-in]
   (read-jar-archive #(classpath-record (location-info obj-in %))
                     obj-in)))

;; ------------------------------------------------------------------------------------------------
;; Classpath: Namespace Sequence
;; ------------------------------------------------------------------------------------------------

(def is-classpath-record? #(::pt/classpath-record? (meta %)))
(def is-directory?        #(when (is-file? %) (.isDirectory %)))
(def is-source-file?      (path-filter path-os only-clojure-exts?))
(def is-artifact?         (path-filter path-os only-artifact-exts?))

(defn classify-obj
  [obj]
  (cond
    (nil? obj)                       :none
    (and (map? obj)
         (is-classpath-record? obj)) :record
    (and (is-file? obj)
         (is-directory? obj))        :raw-directory
    (and (is-file? obj)
         (is-source-file? obj))      :raw-file
    (and (is-file? obj)
         (is-artifact? obj))         :jar-archive
    :else                            :unsupported))

(defn ns-seq
  "Expands a classpath into a seq of maps that contain path info for all available namespaces
   discovered on the classpath. By default, only reads and parses the namespace form in each 
   discovered source file. Defaults to clojure.java.classpath's calculated classpath. Expects 
   classpath-seq to be a seq of File objects."
  ([]
   (ns-seq (jc/classpath)))
  ([[file & more :as _classpath-seq]]
   (let [file-type (classify-obj file)
         lazy-rest (lazy-seq (ns-seq more))]
     ;; (println (format "Expanding [%s] %s" file-type (str file)))
     (case file-type
       :none          nil
       :record        (cons file lazy-rest)
       :raw-file      (cons (read-raw-file file) lazy-rest)
       :jar-archive   (concat (read-jar-content file) lazy-rest)
       :raw-directory (lazy-seq (ns-seq (concat (read-raw-directory file) more)))
       :unsupported   (do (println
                           (format "Warning: Unexpected classpath object [%s] %s"
                                   (class file)
                                   (str file)))
                          lazy-rest)))))

(defn ns-load
  "Given a location info map, return an updated location info map with all forms in the namespace
   fully loaded, populated on the :forms key."
  [{:keys [artifact jar-entry file] :as _location-info}]
  (binding [*enable-parse-all-forms* true]
    (cond file (read-raw-file file)
          (and artifact jar-entry) (read-jar-single-entry artifact jar-entry)
          :else nil)))

(defn ns-index
  "Creates a map of the ns sym (the ns name, aka :ns-id) to the ns location info."
  []
  (->> (ns-seq)
       (filter #(symbol? (:ns-id %)))
       (map (fn [{:keys [ns-id] :as location}]
              (vector ns-id {:ns-location (dissoc location :ns-form :alias-map)})))
       (into {})))
