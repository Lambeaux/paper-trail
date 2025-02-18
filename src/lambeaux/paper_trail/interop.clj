(ns lambeaux.paper-trail.interop
  (:require [lambeaux.paper-trail.interop-proto :as proto]
            [paper.trail :as-alias pt]))

;; ----------------------------------------------------------------------------------------
;; LIST HELPERS
;; ----------------------------------------------------------------------------------------

(defn collcat*
  [f & args]
  (->> args
       (map (fn [x] (if (:expand? (meta x)) x [x])))
       (apply concat)
       (apply f)))

(defn listcat* [& args] (apply collcat* list args))
(defn vectcat* [& args] (apply collcat* vector args))

(defn expand*
  [x]
  (vary-meta x assoc :expand? true))

(defmacro listcat
  "Create a list from the supplied args. Wrap any arg (that's a sequental value) with the 
   (expand) macro to concat its elements into the list."
  [& args]
  `(listcat* ~@args))

(defmacro vectcat
  "Like (listcat) but for vectors."
  [& args]
  `(vectcat* ~@args))

(defmacro expand
  "Provides a hint to (listcat) or (vectcat) that the provided arg is a sequential value 
   and should have its elements appended into the collection."
  [x]
  `(expand* ~x))

;; ----------------------------------------------------------------------------------------
;; CLOJURE IFN GENERATION
;; ----------------------------------------------------------------------------------------

;; look at clojure/lang/IFn.java to see where this constant came from
;; it's the max number of args, excluding the variadic array
(def max-args 20)

(defn generate-apply-to
  "Generates an applyTo implementation similar to the Clojure 
   AFn implementation."
  [seq-sym]
  (let [->seq-call (fn [seq-sym* seq-count*]
                     (->> (range 0 seq-count*)
                          (map (fn [i] (list 'nth seq-sym* i)))
                          (concat ['this])
                          (apply list)))
        seq-count-idxs (range 0 (inc max-args))]
    (list 'applyTo ['this seq-sym]
          (listcat 'case
                   (list 'count seq-sym)
                   (expand (->> seq-count-idxs
                                (map (partial ->seq-call seq-sym))
                                (interleave seq-count-idxs)))
                   (listcat (expand (->seq-call seq-sym max-args))
                            (list 'into-array 'Object (list 'drop max-args seq-sym)))))))

(defn generate-ifn-sigs
  "Generate defrecord signatures for implementing IFn, where all the
   impls delegate to a fn call named by fsym."
  [fsym]
  (let [i->args (fn [i]
                  (->> (range 1 (inc i))
                       (mapv #(symbol (str "arg" %)))))
        i->impl (fn [i]
                  (let [args (i->args i)]
                    (list 'invoke
                          (into ['this] args)
                          (list fsym 'this args))))]
    (listcat (expand
              (map i->impl (range 0 (inc max-args))))
             (let [args (i->args max-args)]
               (list 'invoke
                     (conj (into ['this] args) 'var-args)
                     (list fsym 'this (list 'concat
                                            args
                                            (list 'into [] 'var-args)))))
             (generate-apply-to 'coll))))

(defmacro defcallable
  "Like defrecord but provides an impl of IFn."
  [name fields & body]
  (let [ifn-impl (generate-ifn-sigs 'proto/call)]
    `(defrecord ~name ~fields
       ~@body
       clojure.lang.IFn
       ~@ifn-impl)))

;; ----------------------------------------------------------------------------------------
;; FN PROTOS / RECORDS
;; ----------------------------------------------------------------------------------------

(comment
  #{java.io.Serializable
    java.util.Comparator
    clojure.lang.IObj
    clojure.lang.IFn
    clojure.lang.Fn})

(defcallable SourceFetchFn  []
  proto/InterpretedFn
  (call [this args] {:type (type this) :args args}))

(defcallable SourceCachedFn  []
  proto/InterpretedFn
  (call [_this _args] (throw (ex-info "Not implemented" {}))))

;; ----------------------------------------------------------------------------------------
;; REFLECTIVE EXPLORATION / UTILS
;; ----------------------------------------------------------------------------------------

(def ->class-data
  "Get some interesting metadata for a given Java object."
  (let [ks [:name :package :superclass :interfaces]]
    (fn [obj]
      (let [obj* (if (class? obj)
                   obj
                   (class obj))]
        (select-keys (bean obj*) ks)))))

(defn all-supers
  "Get the super class chain for a given Java object, excluding the
   the common ancestor java.lang.Object of all Java objects."
  [obj]
  (loop [[c :as class-datas] (list (->class-data obj))]
    (let [super* (:superclass c)]
      (if (= super* java.lang.Object)
        (into [] (reverse class-datas))
        (recur (conj class-datas (->class-data super*)))))))

(defn all-interfaces
  "Returns the set of all interfaces implemented by obj."
  [obj]
  (into #{} (mapcat :interfaces (all-supers obj))))

;; ----------------------------------------------------------------------------------------
;; INTEROP NOTES
;; ----------------------------------------------------------------------------------------

(comment

  "JAVA INTEROP NOTES, https://clojure.org/reference/java_interop"

  "----------------------------------------------------------------------"

  "Random note for dynamically wrapping/generating defrecords. Class instances
   contain a ton of useful info, such as what interfaces the class implements."
  (select-keys (bean String)
               [:simpleName :packageName :typeName :interfaces])

  "---------- CLASS ACCESS ----------"

  Object              "Classname - either from java.lang or imported package"
  java.util.Map       "fully qualified Classname - always valid"
  java.util.Map$Entry "Classname$InnerClass"

  ;; The reader won't recognize the following until I bump Clojure to 1.12
  ;; Object/1            "Classname/N - array type of N dimensions, added in 1.12"
  ;; int/1               "primitive/N - array type of N dimensions, added in 1.12"

  "---------- MEMBER ACCESS ----------"

  ;; The following 4 cases are handled by macroexpand
  (.contains "Hi there" "there")        "(.instanceMember instance args*)"
  ;; Not sure about the following case, are they trying to say you can access instance
  ;; members on non-instances (which makes no sense) or the CLASS instance itself (makes
  ;; more sense) ?? Probably the latter.
  (.getTypeName String)                 "(.instanceMember Classname args*), class instance itself"
  (.-x (java.awt.Point. 1 2))           "(.-instanceField instance)"
  (System/getProperty "user.dir")       "(Classname/staticMethod args*)"

  ;; This case is not handled by macroexpand
  Math/PI                               "Classname/staticField"

  ;; This case works on 1.11.1, revisit in 1.12
  (String/.contains "Hi there" "there") "(Classname/.instanceMethod instance args*), added in 1.12"

  "---------- INTERPRETER SUPPORT: SYMBOLS AS CLASSES ----------"

  (resolve 'Object)  "This works"
  (resolve 'Math)    "This works"
  (resolve 'Math/PI) "This does NOT work, use the below fns as ideas to resolve this"

  (defn resolve-field
    "Rough attempt at static field resolution. I'm not keen on the use of reflection,
     because it absolutely kills execution time, but this is just a proof of concept."
    [sym]
    (let [bean* #(merge (bean %) {::pt/original-obj %})
          sym-ns (symbol (namespace sym))
          ns-res (resolve sym-ns)]
      (when (class? ns-res)
        (let [field (->> (bean* ns-res)
                         (:fields)
                         (mapv bean*)
                         (filter #(= (name sym) (:name %)))
                         (first)
                         (::pt/original-obj))]
          (.get field nil)))))

  (resolve-field 'Math/PI)
  (resolve-field 'Math/E)
  (resolve-field 'System/out)

  (defn resolve-field*
    "Oh wait. One potentially obvious solution. Just rewrite the symbol as a dot form."
    [sym]
    (let [sym-ns (symbol (namespace sym))
          ns-res (resolve sym-ns)]
      (when (class? ns-res)
        (list '. sym-ns (symbol (name sym))))))

  (resolve-field* 'Math/PI)
  (resolve-field* 'Math/E)
  (resolve-field* 'System/out)

  (binding [*out* (java.io.PrintWriter. System/out)]
    (println "Math/PI Clojure:")
    (time Math/PI)
    (println)
    (println "Math/PI Paper Trail:")
    (time (resolve-field 'Math/PI))
    (println)
    (println "Math/PI Cached:")
    (time (get {'Math/PI 3.141592653589793} 'Math/PI))
    (println)
    (println "Math/PI Dot Form:")
    (time (. Math PI))
    (println)
    (println "Math/PI Dot Form Eval'd:")
    (time (eval '(. Math PI)))
    (println)
    (println "String/contains Dot Form:")
    (time (. "Hi there" contains "there"))
    (println)
    (println "String/contains memfn:")
    (time ((memfn contains substr) "Hi there" "there"))
    (println))

  "---------- INTERPRETER SUPPORT: DOT NOTATION ----------"

  "Need to enhance the interpreter to support the dot special form: "
  (. "Hi there" contains "there")

  "Some options for providing dot support: "
  (eval '(. "hi" contains "hi"))      "Use eval, not ideal, produces undesireable overhead"
  ((memfn contains substr) "hi" "hi") "Use memfn, great, but shifts problem to fn handling"

  "We probably need type hinting for the memfn approach: "
  (let [f (memfn ^Boolean contains ^String substr)]
    (f "hi" "h"))

  "What if, for fn handling, we create our own defrecord that implements all interfaces?"
  (let [f (fn [i] (inc i))] (all-interfaces f))

  "Note that most of the above interop syntax is handled via macroexpand: "
  (macroexpand '(.contains "Hi there" "there"))
  (macroexpand '(.getTypeName String))
  (macroexpand '(.-x (java.awt.Point. 1 2)))
  (macroexpand '(System/getProperty "user.dir"))

  "...With an exception: the issue here is addressed above"
  (macroexpand 'Math/PI)

  "...Take note: this expands correctly on 1.11.1, revisit on 1.12"
  (macroexpand '(String/.contains "Hi there" "there")))
