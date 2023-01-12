our swaggger docs contain information about which end-points call
which federation end-points internally.  this command line tool
extracts that information from the swagger json and converts it into a
dot file.

### try it out

```
cabal run fedcalls
ls wire-fedcalls.*  # these names are hard-coded (sorry!)
dot -Tpng fedcalls.dot > fedcalls.png
```

`dot` layouts only work for small data sets (at least without tweaking).  for a better one paste into [sketchvis](https://sketchviz.com/new).

### links

- `./example.png`
- https://sketchviz.com/new
- https://graphviz.org/doc/info/lang.html
- `/libs/wire-api/src/Wire/API/MakesFederatedCall.hs`

### swagger-ui

you can get the same data for the public API in the swagger-ui output.  just load the page, open your javascript console, and type:

```
window.ui.getConfigs().showExtensions = true
```

then drop down on things like normal, and you'll see federated calls.
