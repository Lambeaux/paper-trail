(ns lambeaux.paper-trail.classpath
  (:require [clojure.java.classpath :as jc]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [edamame.core :as ed])
  (:import [java.io File]
           [java.util.jar JarFile JarEntry JarInputStream]))

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
