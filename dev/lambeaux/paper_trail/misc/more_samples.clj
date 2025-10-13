(ns lambeaux.paper-trail.misc.more-samples)

(def some-value 100)

(defn add
  ([x y]
   (+ x y))
  ([x y z]
   (+ x y z)))

(defn seq-process
  ([coll]
   (or coll (->> [0 1 2 3 4 5 6 7 8 9]
                 (mapv inc)
                 (filterv even?)
                 (mapv (partial * 2))))))

(defn seq-process-lazy
  ([coll]
   (or coll (->> (range 0 10)
                 (map inc)
                 (filter even?)
                 (mapv (partial * 2))))))
