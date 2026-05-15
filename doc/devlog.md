
# Dev Log

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
