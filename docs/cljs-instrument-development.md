To work on the function instrumentation for ClojureScript clone the malli repository and:

```bash
npm i
./node_modules/.bin/shadow-cljs watch instrument
```

Open an nREPL connection from your favorite editor to the port located in `.shadow-cljs/nrepl.port`

Open a browser to `http://localhost:8000`

the port is set in the `shadow-cljs.edn` file should you wish to change it.


In your editor evaluate:

`(shadow/repl :instrument)`

The dev-time code is located in the file: `app/malli/instrument_app.cljs`.
