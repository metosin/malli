# ClojureScript Function Instrumentation

Function instrumentation is also supported when developing ClojureScript browser applications.

Things work differently from the Clojure version of instrumentation because there are no runtime Vars in ClojureScript and thus the 
instrumentation must happen at compile-time using macros.
The macro will emit code that `set!`s the function at runtime with a version that validates the function's inputs and outputs
against its declared malli schema.

# Dev Setup

For the best developer experience make sure you install the latest version of binaryage/devtools and use a chromium based browser:

https://clojars.org/binaryage/devtools

if you are using shadow-cljs just ensure this library is on the classpath.

The rest of the guide assumes use of shadow-cljs as the instrumentation tooling works best with it.

To enable instrumentation of ClojureScript builds, we add shadow-cljs build hooks and include a malli instrumentation
macro in our entry namespace.

To enable optimal development-time instrumentation shadow-cljs build-hooks are used to track the namespaces you make edits
to as you develop. The instrumentation tooling will only re-instrument the edited namespaces after hot-reloading which should have significant
time savings on codebases with lots of instrumented functions.

Here is an example build config:

```clojure
{...
 :builds
 {:my-build   ; <--- this is your build-id
  {:target :browser
   :modules {:app {:init-fn your-app.entry-ns/init}}
   :build-hooks [(malli.dev.shadow-cljs-instrument-hooks/configure-hook :my-build) ; <-- needs to match the build-id you want to instrument
                 (malli.dev.shadow-cljs-instrument-hooks/compile-flush-hook)]
...}
```

Please note that you must pass the build-id keyword you want to instrument, this is to handle the case where you have multiple
builds being compiled at once and the hook will get messages for all of them. Because of this, instrumentation is intended
to only be applied to one active build only.
In the future the edited namespaces could be tracked per build to get rid of this limitation, but this would have its own tradeoffs, so
for now only instrumenting one build at a time is supported with the build hooks.

Now, in your application's entry namespace you need to tell the compiler to always reload this namespace so that the macro will rerun when
you change schemas and function definitions in other namespaces while developing.

We do this with the `{:dev/always true}` metadata on the namespace:

(this was pointed out by Thomas Heller [here](https://clojureverse.org/t/problem-using-malli-clojurescript-instrumentation-and-shadow-cljs/8612/2).
  If you're still running into stale code issues during development you can try requiring all namespaces in a preload like he suggests in that comment)

```clojure
(ns co.my-org.my-app.entry
  {:dev/always true}
  (:require [malli.dev.cljs :as md]))
```

and require the `malli.dev.cljs` namespace.

In your init function before rendering your application invoke `malli.dev.cljs/start!`

```clojure
(defn ^:export init [] 
  (md/start!)
  (my-app/mount!)
```

When you save source code files during development and new code is hot-reloaded the non-instrumented versions will now 
overwrite any instrumented versions.

Thus, to instrument the newly loaded code with shadow-cljs we can use the [lifecylce hook](https://shadow-cljs.github.io/docs/UsersGuide.html#_lifecycle_hooks)
`:after-load` by adding metadata to a function and this time invoking `malli.dev.cljs/refresh!` which will only instrument
the updated namespaces since the last compile:

```clojure
(defn ^:dev/after-load reload []
  (md/refresh!)
  (my-app/mount!))
```

It is useful to understand what is happening when you invoke `(malli.dev.cljs/start!)` (and `malli.dev.cljs/refresh!`)

The line where `start!` lives in your code will be replaced by a block of code that looks something like:

```clojure
(set! your-app-ns/a-function
   (fn [& args] 
   ;; validate the args against the input schema
   ;; invoke the function your-app-ns/a-function and validate the output against the output schema
   ;; return the output
   )
;; assuming an implementation in your-app-ns like:
(defn a-function 
  {:malli/schema [:=> [:cat :int] :string]}
  [x] 
  (str x))
```

(you can see what is actually output here: https://github.com/metosin/malli/blob/400dc0c79805028a6d85413086d4d6d627231940/src/malli/instrument/cljs.clj#L69)

And this is why the order of loaded code will affect the instrumented functions. If the code for `your-app-ns/a-function`
is hot-reloaded and the `start!` call is never invoked again, the function will no longer be instrumented.

## Errors in the browser console

When you get a schema validation error and instrumentation is on you will see an exception in the browser devtools.

A validation error looks like this:

<img src="img/cljs-instrument/cljs-instrument-error-collapsed.png"/>

If you click the arrow that is highlighted in the above image you will see the error message:

<img src="img/cljs-instrument/cljs-instrument-error-expanded.png"/>

and if you click the arrow highlighted in this above image you will see the stracktrace:

<img src="img/cljs-instrument/cljs-instrument-stacktrace-expanded.png"/>

the instrumented function is the one with the red rectangle around it in the image above.

If you click the filename (`instrument_app.cljs` in this example) the browser devtools will open a file viewer at the problematic call-site.
