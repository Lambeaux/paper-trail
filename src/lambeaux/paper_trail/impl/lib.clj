;; Copyright (c) Steven Lombardi and Contributors. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (https://choosealicense.com/licenses/epl-1.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
(ns lambeaux.paper-trail.impl.lib)

(def ^:dynamic *assert-gen*
  (boolean true))

(defmacro assert*
  "Like clojure.core/assert but throws RuntimeException instead of AssertionError.
   This is useful when you want your assertions to be handled by your default error
   handling / exception catching logic."
  ([x]
   (when *assert-gen*
     `(when-not ~x
        (throw (new RuntimeException (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert-gen*
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
