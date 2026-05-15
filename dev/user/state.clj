(ns user.state
  (:require [clojure.tools.namespace.repl :as tnr]))

(tnr/disable-unload!)
(defonce tap-log (atom []))
