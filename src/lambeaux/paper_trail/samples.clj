(ns lambeaux.paper-trail.samples)

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
