# Linting

## HLint

To run [HLint](https://github.com/ndmitchell/hlint) you need it's binary, e.g.
by executing:

```sh
nix-shell -p hlint
```

To run it on the whole project (Warning: This takes a long time!):

```sh
hlint .
```

To run it on a sub-project:

```sh
hlint services/federator
```

## Stan

To run [Stan](https://github.com/kowainik/stan), you need it's binary compiled
by the same GHC version as used in the project.

```sh
nix-shell -p haskell.packages.ghc884.stan
```

Stan depends on [*hie*](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)
database files that are created during compilation. To generate them for all
packages add this to your `cabal.project.local` file:

```
package *
    ghc-options: -fwrite-ide-info -hiedir=.hie
```

Of course, you can append the `ghc-options` to the respective entry of a package or
add a new one:

```sh
package cargohold
    ghc-options: -fwrite-ide-info -hiedir=.hie
```

To analyze a sub-project with stan:

```sh
cd services/cargohold
stan
```
