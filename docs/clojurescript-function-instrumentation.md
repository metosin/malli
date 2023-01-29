# ClojureScript Function Instrumentation

Function instrumentation is also supported when developing ClojureScript browser applications.

The implementation works by collecting function schemas using a ClojureScript macro which registers the function schemas
available in the application.

Instrumentation happens at runtime in a JavaScript runtime. The functions to instrument are replaced by instrumented versions.

# Dev Setup

For the best developer experience make sure you install the latest version of binaryage/devtools and use a chromium based browser:

https://clojars.org/binaryage/devtools

if you are using shadow-cljs just ensure this library is on the classpath.

For an application that uses React.js such as Reagent you will typically declare an entry namespace and init function in your `shadow-cljs.edn` config like so:

```clojure
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

To instrument the newly loaded code with shadow-cljs we can use the [lifecylce hook](https://shadow-cljs.github.io/docs/UsersGuide.html#_lifecycle_hooks)
`:after-load` by adding metadata to a function and invoking `malli.dev.cljs/start!` again:

```clojure
(defn ^:dev/after-load reload []
  (md/start!)
  (my-app/mount!))
```

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

# Clj-Kondo config

Running `malli.dev` instrumentation also emits [clj-kondo](https://github.com/metosin/malli#clj-kondo) type configs for all `defn`s,
enabling basic static type checking/linting for the instrumented functions.

This is currently only supported when using shadow-cljs.

The config is determined at runtime by the browser's JavaScript VM so cannot be performed statically. Your application must be
executed in orer to deal with dynamic schemas that may show up in function metadata.

To enable this feature, in your `shadow-cljs.edn` file add the following build [hook](https://shadow-cljs.github.io/docs/UsersGuide.html#build-hooks):

```clojure
:build-hooks [(malli.dev.shadow-cljs-hooks/kondo-hook :main)]
```

where `:main` is the module id of your application.

The build id is used to handle cases where multiple shadow-cljs compilations are running simultaneously.
Currently the setup only supports adding this hook to one running module at a time.

After you setup dev-time instrumentation and hot-reload your app you should see your kondo config file present on the filesystem.

# Release builds

You have a few options to ensure that schemas and tooling only used during development don't end up in release builds

1. Put all dev tooling in a preload
2. Use the same app entry for dev and release builds but alias a noop ns for releases.
2. Use a separate entry for dev and release builds and use a script to change them in the shadow-cljs config.

## Use a preload file

The technique was described by Thomas Heller [here](https://clojureverse.org/t/problem-using-malli-clojurescript-instrumentation-and-shadow-cljs/8612/2)
and is adapted here:

"The best way to go about this is via `:preloads` with a custom namespace created for it."

Example:

```clojure
(ns com.my-org.client.preload
  {:dev/always true}
  (:require
    [my.app] ;; must require all namespace here that potentially get instrumented
    [my.app.util]
    [malli.instrument :as mi]))

(mi/instrument!)
```

Then in your build config include the preload for the build you want using `:devtools {:preloads [com.my-org.client.preload]}`.

Like so:
```clojure
,,,
{:main
 {:target          :browser
  :output-dir      "resources/public/js/main"
  :asset-path      "js/main"
  :modules         {:main {:init-fn com.my-org.client.entry/init}}
  :devtools        {:preloads [com.my-org.client.preload]}    ;;      <------------
  :closure-defines {malli.registry/type "custom"}
  :release         {:build-options {:ns-aliases
                                    {malli.dev.cljs                   malli.dev.cljs-noop
                                     com.my-org.client.malli-registry com.my-org.client.malli-registry-release}}}
,,,
```

From Thomas' post:

The `{:dev/always true}` metadata on the namespace ensures that this namespace is always recompiled.
Although note that this can make your build a lot slower.
It isn’t strictly necessary if the macro doesn’t emit changing code but since there is no reliable way to know what the macro does (from the shadow-cljs side) it might be best to always run it.

The `my.app` require ensures that the preload is actually compiled after all your app namespaces have been compiled.
I know "preload" isn’t the best name here but you must do this to ensure that other namespaces aren’t still compiling when the macro runs.

----

Using this setup you will not have development-time tooling and schemas in your release build as `preloads` are not included
in release builds.

## Use a release no-op namespace

To use the included `malli.dev.cljs-noop` namespace which will result in adding 9 bytes to your production build instead
of pulling in all the instrumentation code.

With shadow-cljs you can configure this like so using the `ns-aliases` build option:

```clojure
,,,
 {:main
  {:target          :browser
   :output-dir      "resources/public/js/main"
   :asset-path      "js/main"
   :modules         {:main {:init-fn com.my-org.client.entry/init}}
   :closure-defines {malli.registry/type "custom"}
   :release         {:build-options {:ns-aliases
                                     {malli.dev.cljs                   malli.dev.cljs-noop
                                      com.my-org.client.malli-registry com.my-org.client.malli-registry-release}}}
,,,
```

This example also shows how you can include a separate malli registry with only the schemas you need for your production
release.

This way you can include schemas that are only used for instrumenting functions during development, for example, but for a release include
only the schemas needed to power functionality of your application.

It should be noted also that metadata declared on function vars in ClojureScript is excluded by default by the ClojureScript
compiler. The function var metadata is only included in a build if you explicitly invoke a static call to it such as:
```clojure
(meta (var com.my-org.some.ns/a-fn))
```
Unless you explicitly ask for it, function var metadata will not be included in your compiled JS, so annotating functions
with malli schemas in metadata has no costs to your builds.

## `ns-alias` release namespaces

Another strategy to get optimal CLJS releases is by having a completely separate entry namespace for release builds.

Then you have to either include a separate shadow-cljs config entry or use a Clojure build script like the following to compile
your build which will let you have one config build entry in `shadow-cljs.edn` for your app:

```clojure
(ns fe-build
  (:require
    [shadow.cljs.devtools.config :as config]
    [shadow.cljs.build-report :as report]
    [shadow.cljs.devtools.api :as sh]))

;; this code was figured out by inspecting the source of the shadow.cljs.devtools.api and related namespaces.

;; replace the entry namespace
(defn get-config [build-id]
  (get-in
    (assoc-in
      (config/load-cljs-edn!)
      [:builds build-id :modules :main :init-fn] 'com.my-org.client.release-entry/init)
    ;; ^ note the :main module name may be different for your app
    [:builds build-id]))

(defn release-build [{:keys [id] :or {id :main}}]
  (sh/with-runtime
    (let [build-config (get-config id)]
      (sh/release* build-config {})))
  :done)

(defn build-report [{:keys [id] :or {id :main}}]
  (report/generate (get-config id)
    {:print-table true
     :report-file "fe-report.html"})
  :done)
```
You would include this somewhere on your classpath (like under `src/build`) and then invoke it from the command line or via a REPL:

```bash
clojure -X:fe-build fe-build/release-build :id :main

# and for the build report:

clojure -X:fe-build fe-build/build-report :id :main
```
where `:fe-build` alias has:

```clojure
:fe-build {:extra-paths ["src/build"]}
```
