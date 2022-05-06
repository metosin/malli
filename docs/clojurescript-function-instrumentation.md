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

For an application that uses React.js such as Reagent you will typically declare an entry namespace and init function in your `shadow-cljs.edn` config like so:

```clj
{...
:modules {:app {:entries [your-app.entry-ns]
:init-fn your-app.entry-ns/init}}
...}
```

In your application's entry namespace you need to tell the compiler to always reload this namespace so that the macro will rerun when
you change schemas and function definitions in other namespaces while developing.

We do this with the `{:dev/always true}` metadata on the namespace:

(this was pointed out by Thomas Heller [here](https://clojureverse.org/t/problem-using-malli-clojurescript-instrumentation-and-shadow-cljs/8612/2).
  If you're still running into stale code issues during development you can try requiring all namespaces in a preload like he suggests in that comment)

```clj
(ns co.my-org.my-app.entry
  {:dev/always true}
  (:require [malli.dev.cljs :as md]))
```

and require the `malli.dev.cljs` namespace.

In your init function before rendering your application invoke `malli.dev.cljs/start!`

```clj
(defn ^:export init [] 
  (md/start!)
  (my-app/mount!)
```

When you save source code files during development and new code is hot-reloaded the non-instrumented versions will now 
overwrite any instrumented versions.

To instrument the newly loaded code with shadow-cljs we can use the [lifecylce hook](https://shadow-cljs.github.io/docs/UsersGuide.html#_lifecycle_hooks)
`:after-load` by adding metadata to a function and invoking `malli.dev.cljs/start!` again:

```clj
(defn ^:dev/after-load reload []
  (md/start!)
  (my-app/mount!))
```

It is useful to understand what is happening when you invoke `(malli.dev.cljs/start!)`

The line where `start!` lives in your code will be replaced by a block of code that looks something like:

```clj
(set! your-app-ns/a-function
   (fn [& args] 
   :; validate the args against the input schema
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
