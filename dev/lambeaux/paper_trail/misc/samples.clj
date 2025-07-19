(ns lambeaux.paper-trail.misc.samples
  (:require [clojure.java.io :as io]
            [aero.core :as aero]))

(defn simplest-even [num-seq]
  (filter even? (map inc num-seq)))

(defn simplest-odd [num-seq]
  (filter odd? (map inc num-seq)))

(defn simple-transform [num-seq]
  (into (vector) (remove #(< % 10) (filter even? (map inc num-seq)))))

(defn simple-transform-with-threading [num-seq]
  (->> num-seq (map inc) (filter even?) (remove #(< % 10)) (into (vector))))

(defn simple-transform-with-transducers [num-seq]
  (into (vector)
        (comp (map inc) (filter even?) (remove #(< % 10)))
        num-seq))

(defn simple-transform-as-vector [num-seq]
  [(first num-seq)
   (rest num-seq)])

(defn simple-transform-with-nested-vector [num-seq]
  (apply concat [(filterv even? (map inc num-seq))
                 (filterv odd? (map inc num-seq))]))

(defn simple-transform-as-map [num-seq]
  {:first (first num-seq)
   :rest (rest num-seq)})

(defn simple-transform-with-nested-map [num-seq]
  (update-keys {(keyword "a") (filterv even? num-seq)
                (keyword "b") (filterv odd? num-seq)}
               #(keyword (str (name %) "1"))))

(defn composite-transform [num-seq]
  (-> {:evens (simplest-even num-seq)
       :odds (simplest-odd num-seq)}
      (assoc :threading (simple-transform-with-threading num-seq))
      (assoc :transducers (simple-transform-with-transducers num-seq))))

(defn third-party-library-aliased [arglist]
  (-> (io/resource "sample-config.edn")
      (aero/read-config)
      (assoc :args (vec arglist))))

(defn third-party-library-fully-qualified [arglist]
  (-> (clojure.java.io/resource "sample-config.edn")
      (aero.core/read-config)
      (assoc :args (vec arglist))))

(defn fn-with-variadics [& args]
  ;; how would we handle (range) / infinite seqs?
  (interleave (range 1 11) (partition 2 args)))

(defn fn-with-overloads-and-variadics
  ([a] (+ 1 a))
  ([a b] (+ 1 a b))
  ([a b & inputs] (apply + a b inputs)))

(defn fn-with-overloads-and-variadics-not-in-order
  ([a b] (+ 1 a b))
  ([a] (+ 1 a))
  ([a b & inputs] (apply + a b inputs)))

(defn fn-with-seq-destructuring
  [n [x & xs :as coll]]
  (hash-map :n (take n (map-indexed vector coll))
            x xs))

(defn fn-with-map-destructuring
  [{:keys [hi] :samples/keys [bye]
    {:keys [etc] {:as from} :to :as another-name} :another-key
    qual-name :samples/my-name
    :as ctx}]
  [hi bye etc from another-name qual-name ctx])

(defn fn-with-both-destructuring
  [{:keys [a b] 
    [bob steve {:keys [name] :as fred}] :coll
    :as root}]
  [a b bob steve name fred root])