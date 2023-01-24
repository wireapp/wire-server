our swaggger docs contain information about which end-points call
which federation end-points internally.  this command line tool
extracts that information from the swagger json and converts it into
two files: dot (for feeding into graphviz), and csv.

### try it out

```
cabal run fedcalls
ls wire-fedcalls.*  # these names are hard-coded (sorry!)
dot -Tpng wire-fedcalls.dot > wire-fedcalls.png
```

`dot` layouts only work for small data sets (at least without tweaking).  for a better one paste into [sketchvis](https://sketchviz.com/new).

### links

for users:

- blog post explaining the technology: https://reasonablypolymorphic.com/blog/abusing-constraints/index.html
- https://sketchviz.com/new
- https://graphviz.org/doc/info/lang.html

for developers:

- `./example.png`
- [MakesFederatedCall.hs (as of 2023-01-16)](https://github.com/wireapp/wire-server/blob/8760b4978ccb039b229d458b7a08136a05e12ff9/libs/wire-api/src/Wire/API/MakesFederatedCall.hs)
- PRs: https://github.com/wireapp/wire-server/pull/2973, https://github.com/wireapp/wire-server/pull/2940, https://github.com/wireapp/wire-server/pull/2950, https://github.com/wireapp/wire-server/pull/2957

### swagger-ui

you can get the same data for the public API in the swagger-ui output.  just load the page, open your javascript console, and type:

```
window.ui.getConfigs().showExtensions = true
```

then drop down on things like normal, and you'll see federated calls.
