(ns lambeaux.paper-trail.interop-proto)

(defprotocol InterpretedFn
  "Paper Trail's function abstraction."
  (call [this args]))
