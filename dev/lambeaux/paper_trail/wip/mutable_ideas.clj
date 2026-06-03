(ns lambeaux.paper-trail.wip.mutable-ideas
  (:require [lambeaux.paper-trail.wip.interop :as wip-iop])
  (:import  [clojure.lang
             ISeq
             IPersistentCollection
             IPersistentVector]
            [java.lang UnsupportedOperationException]
            [java.io Writer]
            [java.util Collections ArrayList]))

(declare ->ReadOnlySeq ->MutableVector)

(deftype ReadOnlySeq [listView]
  Object
  (toString [_this]
    (pr-str listView))
  IPersistentCollection
  (count [_this]
    (.size listView))
  (cons [_this _v]
    (throw (UnsupportedOperationException.)))
  (empty [_this]
    (->ReadOnlySeq Collections/EMPTY_LIST))
  ;; todo: fix impl later
  (equiv [this obj]
    (= this obj))
  ;; clojure.lang.Seqable
  (seq [this]
    (when (pos? (.size listView))
      this))
  ISeq
  (first [_this]
    (when (pos? (.size listView))
      (.getFirst listView)))
  (next [_this]
    (when (pos? (.size listView))
      (not-empty
       (->ReadOnlySeq (.subList listView 1 (.size listView))))))
  (more [_this]
    (if-not (pos? (.size listView))
      (->ReadOnlySeq Collections/EMPTY_LIST)
      (->ReadOnlySeq (.subList listView 1 (.size listView))))))

(defmethod print-method ReadOnlySeq
  [this ^Writer writer]
  (.write writer (pr-str (.listView this))))

(defmethod print-dup ReadOnlySeq
  [this ^Writer writer]
  (.write writer (pr-str (.listView this))))

(deftype ReadOnlyEntry [k v]
  clojure.lang.IMapEntry
  (getKey [_this] k)
  (getValue [_this] v)
  (key [_this] k)
  (val [_this] v))

(comment
  clojure.lang.APersistentVector
  clojure.lang.IFn
  java.lang.Iterable
  java.lang.Comparable
  java.io.Serializable
  java.util.RandomAccess
  java.util.List
  clojure.lang.IHashEq)

(comment
  clojure.lang.PersistentVector
  clojure.lang.IObj
  clojure.lang.IEditableCollection
  clojure.lang.IReduce
  clojure.lang.IKVReduce)

(comment
  (deftype DelegateList [jvmList]
    ;; --------------------------------------------
    java.lang.Iterable
    (iterator [_this]
      (.iterator jvmList))
    java.util.List
    (add [_this v]
      (.add jvmList v))
    (add [_this idx v]
      (.add jvmList idx v))
    (addAll [_this coll]
      (.addAll jvmList coll))
    (addAll [_this idx coll]
      (.addAll jvmList idx coll))
    (clear [_this]
      (.clear jvmList))
    (contains [_this v]
      (.contains jvmList v))
    (containsAll [_this coll]
      (.containsAll jvmList coll))
    (get [_this idx]
      (.get jvmList idx))
    (indexOf [_this v]
      (.indexOf jvmList v))
    (lastIndexOf [_this v]
      (.lastIndexOf jvmList v))
    (isEmpty [_this]
      (.isEmpty jvmList))
    (listIterator [_this]
      (.listIterator jvmList))
    (listIterator [_this idx]
      (.listIterator jvmList idx))
    (^Object remove [_this ^int idx]
      (.remove jvmList idx))
    (^boolean remove [_this ^Object obj]
      (.remove jvmList obj))
    (removeAll [_this coll]
      (.removeAll jvmList coll))
    (retainAll [_this coll]
      (.retainAll jvmList coll))
    (set [_this idx v]
      (.set jvmList idx v))
    (size [_this]
      (.size jvmList))
    (subList [_this fromIdx toIdx]
      (.subList jvmList fromIdx toIdx))
    (toArray [_this]
      (.toArray jvmList))
    ;; todo: fix after 1.12 upgrade
    (^"[Ljava.lang.Object;" toArray [_this ^"[Ljava.lang.Object;" tarray]
      (.toArray jvmList tarray))))

(deftype MutableVector [jvmList]
  Object
  (toString [_this]
    (pr-str jvmList))
  ;; --------------------------------------------
  IPersistentCollection
  (count [_this]
    (.size jvmList))
  (empty [_this]
    (->MutableVector (ArrayList.)))
  ;; todo: fix impl later
  (equiv [this obj]
    (= this obj))
  ;; writes:
  (cons [this v]
    (.add jvmList v)
    this)
  ;; --------------------------------------------
  IPersistentVector
  (length [_this]
    (.size jvmList))
  ;; writes:
  (assocN [this idx v]
    (let [size* (.size jvmList)]
      (.add jvmList idx v)
      (when (< idx size*)
        (.remove jvmList))
      this))
  ;; --------------------------------------------
  ;; clojure.lang.IPersistentStack
  (peek [_this]
    (.getLast jvmList))
  ;; writes:
  (pop [this]
    (.removeLast jvmList)
    this)
  ;; --------------------------------------------
  ;; clojure.lang.Sequential
  ;; clojure.lang.Associative
  (containsKey [_this k]
    (< k (.size jvmList)))
  (entryAt [_this k]
    (->ReadOnlyEntry k (.get jvmList k)))
  ;; writes:
  (assoc [this k v]
    (let [idx k
          size* (.size jvmList)]
      (.add jvmList idx v)
      (when (< idx size*)
        (.remove jvmList))
      this))
  ;; --------------------------------------------
  ;; clojure.lang.Seqable
  (seq [_this]
    (when (pos? (.size jvmList))
      (->ReadOnlySeq (.subList jvmList 0 (.size jvmList)))))
  ;; --------------------------------------------
  ;; clojure.lang.Reversible
  (rseq [_this]
    (when (pos? (.size jvmList))
      (->ReadOnlySeq (.reversed (.subList jvmList 0 (.size jvmList))))))
  ;; --------------------------------------------
  ;; clojure.lang.Indexed
  (nth [_this idx]
    (.get jvmList idx))
  (nth [_this idx not-found]
    (if-not (and (pos? idx)
                 (< idx (.size jvmList)))
      not-found
      (.get jvmList idx))))

(defmethod print-method MutableVector
  [this ^Writer writer]
  (.write writer (pr-str (.jvmList this))))

(defmethod print-dup MutableVector
  [this ^Writer writer]
  (.write writer (pr-str (.jvmList this))))

(defn mutable-vector
  ([]
   (->MutableVector (ArrayList.)))
  ([coll]
   (->MutableVector (ArrayList. coll))))

(comment
  (defn sample-build
    [coll]
    (->> coll
         ())))

(comment

  clojure.lang.APersistentVector
  clojure.lang.RT
  clojure.lang.Seqable
  clojure.lang.ISeq

  clojure.lang.IPersistentStack
  clojure.lang.IPersistentCollection

  clojure.lang.IPersistentVector
  clojure.lang.Associative
  clojure.lang.IMapEntry
  clojure.lang.Sequential
  clojure.lang.Reversible
  clojure.lang.Indexed

  (->> (wip-iop/all-interfaces clojure.lang.PersistentVector)
       (seq)
       (sort #(compare (.getName %1) (.getName %2)))
       (into []))
  [clojure.lang.IEditableCollection
   clojure.lang.IFn
   clojure.lang.IHashEq
   clojure.lang.IKVReduce
   clojure.lang.IObj
   clojure.lang.IPersistentVector
   clojure.lang.IPersistentCollection
   clojure.lang.IReduce
   java.io.Serializable
   java.lang.Comparable
   java.lang.Iterable
   java.util.List
   java.util.RandomAccess]

  (->> (wip-iop/all-interfaces clojure.lang.PersistentHashMap)
       (seq)
       (sort #(compare (.getName %1) (.getName %2)))
       (into []))
  [clojure.lang.IEditableCollection
   clojure.lang.IFn
   clojure.lang.IHashEq
   clojure.lang.IKVReduce
   clojure.lang.IMapIterable
   clojure.lang.IObj
   clojure.lang.IPersistentMap
   clojure.lang.MapEquivalence
   java.io.Serializable
   java.lang.Iterable
   java.util.Map])
