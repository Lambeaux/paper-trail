(ns lambeaux.paper-trail.api)

(defn eval*
  "Like clojure.core/eval but uses the paper-trail interpreter to produce
   the result."
  [form]
  ::not-implemented)

(defn tracer
  "Creates a new tracer with clean state, using either default configuration,
   or the supplied opts."
  ([]
   ::not-implemented)
  ([opts]
   ::not-implemented))

(defn with-body
  "Returns a trace-site for tracing the provided body."
  [& body]
  ::not-implemented)

(defn with-body-opts
  "Returns a trace-site for tracing the provided body, configured using the
   provided opts."
  [opts & body]
  ::not-implemented)

(defn with-body-state
  "Returns a trace-site for tracing the provided body, whose configuration and
   state are managed using the provided tracer."
  [tracer & body]
  ::not-implemented)

(defn with-fn
  "Returns a trace-site for tracing the provided function, called with the
   provided args."
  [f & args]
  ::not-implemented)

(defn with-fn-opts
  "Returns a trace-site for tracing the provided function, called with the
   provided args, and configured using the provided opts."
  [opts f & args]
  ::not-implemented)

(defn with-fn-state
  "Returns a trace-site for tracing the provided function, called with the 
   provided args, and whose configuration and state are managed using the 
   provided tracer."
  [tracer f & args]
  ::not-implemented)

(defn clear-buffer!
  "Signals to the trace consumer that all trace logs for the last completed
   trace operation (for the current thread) should be dropped. If another
   trace operation begins, and the buffer has beeen neither cleared nor flushed,
   it is cleared (currently subject to change)."
  [tracer]
  ::not-implemented)

(defn flush-buffer!
  "Signals to the trace consumer that all trace logs for the last completed 
   trace operation (for the current thread) should be flushed to the destination. 
   If another trace operation begins, and the buffer has been neither cleared nor 
   flushed, it is cleared (currently subject to change)."
  [tracer]
  ::not-implemented)

(defn trace
  "Given a trace-site, performs the trace, sending all trace logs to the configured
   trace consumer, and returns the as-evaluated-by-Clojure return value of the
   interpreted code. 
   
   WARNING: Does not clear or flush the buffer. If another trace operation begins, 
   and the buffer has been neither cleared nor flushed, it is cleared (currently 
   subject to change)."
  [trace-site]
  ::not-implemented)

(defn trace-value
  "Given a trace-site, performs the trace, sending all trace logs to the configured
   trace consumer, and returns the as-evaluated-by-Clojure return value of the
   interpreted code.
   
   Always flushes the buffer."
  [trace-site]
  ::not-implemented)

(defn trace-report
  "Given a trace-site, performs the trace, sending all trace logs to the in-memory
   trace consumer, and returns the full trace report for the interpreted code. 

   Might need to return the report in either a delay or promise. Not sure yet.
   
   Always flushes the buffer (might not be relevant, I'm not sure if in-memory 
   consumer will even need a flush; might still be useful for a long running local
   process or REPL)."
  [trace-site]
  ::not-implemented)

(defn observe
  "Given a trace-site, performs the trace, sending all trace logs to the configured
   trace consumer if pred returns logical true. Returns the as-evaluated-by-Clojure 
   return value of the interpreted code. It's expected that pred is a function of
   two arguments, 'val' and 'err', where 'val' is the successful return value of the 
   trace and 'err' is an exception if one was thrown during the trace.

   If an exception was thrown during the trace, it is rethrown, not swallowed.

   Only one of 'val' or 'err' should be nil, but it's possible for both to be nil
   if the trace succeeds without error and the return value of the trace is nil.
   If an exception was thrown, 'err' will never be nil.
   
   Useful for monitoring Clojure execution and only sending logs when something
   interesting or exceptional happens, such as a return value failing validation or
   an unexpected error being thrown."
  [pred trace-site]
  (let [p (promise)]
    (try
      (deliver p [(trace trace-site) nil])
      (first (deref p))
      (catch Exception e
        (deliver p [nil e])
        (throw e))
      (finally
        (if (apply pred (deref p))
          (flush-buffer! trace-site)
          (clear-buffer! trace-site))))))

(defn replay
  "Executes the last trace that occurred, with the same parameters, and without additional
   mutations to Clojure's stateful constructs (Vars, Atoms, Refs, Agents, etc). Additional
   functions can be added to the config to be marked as 'stateful' (e.g. such as HTTP client
   libraries that call REST services) and they will be protected as well."
  [tracer]
  ::not-implemented)
