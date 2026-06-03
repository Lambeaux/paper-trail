
# Dev Log

## Features

### Destructuring

#### How explicit/verbose should commands be?

Consider the following form:

```clojure
(let [[x y] (vector 1 2 3)] (vector x y))
```

In an early implementation of destructuring, the above input would generate multiple `bind-name`
commands; one for each symbol in the destructuring. The first would resolve fine:

```clojure
;; note: working snippet as of 05/30/2026
(-> '(let [[x y] (vector 1 2 3)] (vector x y))
    (conf/compare-eval*)
    (second)
    (get-in [:outcome :lambeaux.paper-trail/result :data])
    (update :context (fn [ctx]
                       (assoc ctx :last-command 
                              (-> ctx :command-meta :preview-last first))))
    (get :context)
    (select-keys [:last-command :source-scope]))
{:last-command
 {:path [0], 
  :cmd-idx 11, 
  :form-depth 0, 
  :bind-id x, 
  :action :bind-name, 
  :bind-from :call-stack, 
  :in-macro? false},
 :source-scope {x (1)}}
```

The second one would blow up:

```clojure
(let [result (-> '(let [[x y] (vector 1 2 3)] (vector x y))
                  (conf/compare-eval*)
                  (second)
                  (get-in [:outcome :lambeaux.paper-trail/result]))]
   {:next-command (-> result :data :context :command-meta :preview-next first)
    :cause (:cause result)})
{:next-command {:path [1], 
                :form-depth 0, 
                :bind-id y, 
                :action :bind-name, 
                :bind-from :call-stack, 
                :in-macro? false},
 :cause
 {:type :lambeaux.paper-trail/ex-comparable,
  :clazz java.lang.IllegalStateException,
  :message "No valid stack frame to pop: ()",
  :data nil,
  :cause nil}}
```

Here are the commands for the form; just the first 14 commands, covering the lifecycle of the first
two stack frames:

```clojure
(ptg/generate '(let [[x y] (vector 1 2 3)] (vector x y)))
({:form-meta {:line 295, :column 16}, :form-depth 0, :type :special, :op let, :action :begin-form}
 {:form-depth 0, :action :stack-push-frame}
 {:form-meta {:line 295, :column 28}, :form-depth 1, :type :fn, :op vector, :action :begin-form}
 {:form-depth 1, :action :stack-push-frame}
 {:form-depth 1, :action :scalar, :form vector, :eval? true}
 {:form-depth 1, :action :scalar, :form 1, :eval? true}
 {:form-depth 1, :action :scalar, :form 2, :eval? true}
 {:form-depth 1, :action :scalar, :form 3, :eval? true}
 {:form-depth 1, :op vector, :arg-count 3, :action :invoke-fn}
 {:form-depth 1, :action :stack-pop-frame}
 {:form-meta {:line 295, :column 28}, :form-depth 1, :type :fn, :op vector, :action :end-form}
 {:path [0], :form-depth 0, :bind-id x, :action :bind-name, :bind-from :call-stack}
 {:path [1], :form-depth 0, :bind-id y, :action :bind-name, :bind-from :call-stack}
 {:form-depth 0, :action :stack-pop-frame}
 ...)
```

Besides the frame pops that happen per the above commands, there is another special case inside
`lambeaux.paper-trail.impl.executor/process-bind-name` where the stack is popped:

```clojure
(model/default-update ctx (if (not= :call-stack bind-from)
                                keyval-pairs
                                (conj keyval-pairs :call-stack (stack/frame-pop call-stack))))
```

Without any destructure tests, the current test suite fully passes with this special case:

```clojure
(r/with-std-out
  (t/run-all-tests))
{:test 19, :pass 238, :fail 0, :error 0, :type :summary}
```

But remove the special case:

```clojure
(model/default-update ctx (if (not= :call-stack bind-from)
                                keyval-pairs
                                keyval-pairs))
```

And tests fail:

```clojure
(r/with-std-out
  (t/run-all-tests))
{:test 19, :pass 233, :fail 5, :error 0, :type :summary}
```

But only for a very specific class of forms:

```clojure
(->> (with-out-str
       (binding [t/*test-out* *out*]
         (t/run-all-tests)))
     (str/split-lines)
     (filterv (complement str/blank?))
     (filterv #(str/starts-with? % "Test "))
     (mapv (fn [test-line]
             (let [form-splits (r/split-forms test-line)]
               {:message (clojure.string/join \space (butlast form-splits))
                :form (clojure.edn/read-string (last form-splits))}))))
[{:message
  "Test (try) with (finally) when nothing gets thrown using basic values",
  :form
  (deref
   (let [x (atom 1)]
     (try
       (swap! x inc)
       (swap! x inc)
       x
       (finally
         (swap! x inc)))))}
 {:message
  "Test (try) with (finally) when nothing gets thrown using basic values",
  :form
  (deref
   (let [x (atom 1)]
     (try
       (swap! x inc)
       (swap! x inc)
       x
       (finally
         (swap! x inc)
         (swap! x inc)))))}
 {:message
  "Test (try) with (finally) when nothing gets thrown using basic values in a nested try",
  :form
  (deref
   (let [x (atom 1)]
     (try
       (swap! x inc)
       (try
         (swap! x inc)
         (swap! x inc)
         x
         (finally
           (swap! x inc)))
       (finally
         (swap! x inc)))))}
 {:message
  "Test (try) with (finally) when nothing gets thrown using basic values in a nested try",
  :form
  (deref
   (let [x (atom 1)]
     (try
       (swap! x inc)
       (try
         (swap! x inc)
         (swap! x inc)
         x
         (finally (swap! x inc) (swap! x inc)))
       (finally
         (swap! x inc)
         (swap! x inc)))))}
 {:message
  "Test (try) with (finally) when nothing gets thrown using basic values in a nested finally",
  :form
  (deref
   (let [x (atom 1)]
     (try (swap! x inc)
          (finally
            (try
              (swap! x inc)
              (swap! x inc)
              x
              (finally
                (swap! x inc)))))))}]
```

## Dev Knowledge

### Confusing Issues

Some notes on things that lead to confusing results but in reality are just user errors.

#### Do not forget to quote inputs

I was seeing discrepancies between my unit tests and my repl verification. I would run tests
like so:

```bash
clj -X:test
```

I would verify at the repl like so:

```clojure
;; clj꞉user꞉> 
(conf/compare-eval*
 (let [x (+ 1 2 3)
       y (let [x (inc x)] (inc x)) z (inc x)]
   (vector x y z)))
```

The tests would fail but the repl verification reported no issues and that the results between
`clojure.core/eval` and `lambeaux.paper-trail.impl.core/evaluate` were a match. This made zero
sense. The issue turned out to be user error. It's been awhile since I was working on this, and
the input forms to `conf/compare-eval*` must be quoted (it's a function, not a macro). Since I
was not quoting the inputs, the forms were being evaluated and their results evaluated again
during the actual test, which would obviously come out equal and hide any discrepancies in the
interpreter's implementation.
