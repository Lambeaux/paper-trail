;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.wip.namespaces
  (:require [clojure.string :as str]
            [lambeaux.paper-trail.impl.util :as ptu]))

(defn ns-path-infer
  "Stopgap until classpath support is done."
  [ns-in]
  (let [dir-proj  (System/getProperty "user.dir")
        dir-class (as-> (ns-name ns-in) $
                    (name $)
                    (str/split $ #"\.")
                    (str/join "/" $)
                    (str/replace $ "-" "_"))]
    ;; TODO: remove assumptions (src and clj)
    (str dir-proj "/src/" dir-class ".clj")))

(defn ns-refers-nocore
  "Like ns-refers but maps to namespaces, not vars. Also removes all entries in clojure.core 
   since that case is handled explicitly elsewhere."
  [ns-in]
  (->> (ns-refers (the-ns ns-in))
       (map (fn [[k v]] (vector k (-> v meta :ns))))
       (remove (fn [[_ v]] (= 'clojure.core (ns-name v))))
       (into {})))

(defn ns-qualify-name
  "Attempts to qualify a symbol sym based on src-ns.
   * If there's nothing operate on, return nil.
   * If sym is not qualified, check if it's referred, else return unchanged.
   * If sym is qualified and the ns exists, return sym unchanged.
   * If sym is qualified by a non-existent namespace, check if it's aliased, else return unchanged."
  [src-ns sym]
  (let [[sym-ns sym-name] (ptu/sym-split sym)]
    (cond
      (not sym-name)          nil
      (not sym-ns)            (if-let [refer-ns (get (ns-refers-nocore src-ns) sym-name)]
                                (symbol (name (ns-name refer-ns))
                                        (name sym-name))
                                sym)
      (ptu/ns-exists? sym-ns) sym
      :else                   (if-let [alias-ns (get (ns-aliases src-ns) sym-ns)]
                                (symbol (name (ns-name alias-ns))
                                        (name sym-name))
                                sym))))

(defn ns-resolve-name
  ([sym]
   (assert (symbol? sym) "sym must be a symbol")
   (ns-resolve-name (namespace sym) (name sym)))
  ([ns-str name-str]
   (ns-resolve-name *ns* ns-str name-str))
  ([src-ns ns-str name-str]
   (ns-resolve-name (constantly nil) src-ns ns-str name-str))
  ([lookup-fn src-ns ns-str name-str]
   (assert (not (str/blank? name-str)) "name-str cannot be blank")
   (assert (or (nil? ns-str)
               (not (str/blank? ns-str))) "ns-str cannot be blank, if provided")
   (let [[sym-ns sym-name] (ptu/sym-split (symbol ns-str name-str))]
     (if sym-ns
       (ns-resolve sym-ns sym-name)
       (or (ns-resolve (the-ns src-ns) sym-name)
           (lookup-fn sym-name)
           (ns-resolve 'clojure.core sym-name))))))
