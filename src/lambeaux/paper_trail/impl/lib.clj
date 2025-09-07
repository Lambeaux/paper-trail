(ns lambeaux.paper-trail.impl.lib)

(defmacro assert*
  "Like clojure.core/assert but throws RuntimeException instead of AssertionError.
   This is useful when you want your assertions to be handled by your default error
   handling / exception catching logic."
  ([x]
   (when *assert*
     `(when-not ~x
        (throw (new RuntimeException (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (throw (new RuntimeException (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

#_(defn assoc-meta
    "Like 'assoc' but operates on obj's metadata."
    ([obj k v]
     (vary-meta obj assoc k v))
    ([obj k v k* v*]
     (vary-meta obj assoc k v k* v*))
    ([obj k v k* v* & kvs]
     (let [f #(apply assoc (concat [% k v k* v*] kvs))]
       (vary-meta obj f))))
