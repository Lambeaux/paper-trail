(ns lambeaux.paper-trail.samples)

(defn simplest [num-seq]
  (filter even? (map inc num-seq)))

(defn simple-transform [num-seq]
  (into (vector) (remove #(< % 10) (filter even? (map inc num-seq)))))

(defn simple-transform-with-threading [num-seq]
  (->> num-seq (map inc) (filter even?) (remove #(< % 10)) (into (vector))))

(defn simple-transform-with-transducers [num-seq]
  (into (vector)
        (comp (map inc) (filter even?) (remove #(< % 10)))
        num-seq))
