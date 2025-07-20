(ns lambeaux.paper-trail.wip.interop-proto)

(defprotocol InterpretedFn
  "Paper Trail's function abstraction."
  (call [this args]))
