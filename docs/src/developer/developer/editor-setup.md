# Editor setup

Reference: {#DevEditor}

This page provides tips for setting up editors to work with the Wire codebase.

## For multiple editors {#DevAll}

### Using Haskell IDE Engine

See [official documentation](https://github.com/haskell/haskell-ide-engine)

In addition, you can generate (and re-generate after changes to stack.yaml) a `hie.yaml` configuration file with

```
make hie.yaml
```

## Emacs {#DevEmacs}

### Jump-to-definition {#DevEmacsJump}

Jump-to-definition is possible with [hasktags][]. First you need to install it and make sure it's on your PATH (if you don't want hasktags to be on your PATH, do `M-x customize-variable haskell-hasktags-path`):

[hasktags]: https://hackage.haskell.org/package/hasktags

```bash
stack install hasktags    # or cabal install hasktags
```

To generate tags, do `M-x haskell-mode-generate-tags`. You can also add a Git hook to regenerate tags on checkout:

```bash
echo "hasktags -e -x ." > .git/hooks/post-checkout
chmod +x .git/hooks/post-checkout
```

To jump to an identifier, press `M-.`. You can also do `C-u M-x xref-find-definitions` to get interactive search through identifiers. Press `M-,` to return to where you were before the jump.

Jump-to-definition is case-insensitive by default, which is probably not what you want. To change that, do `M-x customize-variable tags-case-fold-search`.

By default hasktags only generates tags for the current package. The Wire backend is composed of many packages, and it's useful to be able to jump to any identifier in wire-server. One way to do it is to setup Emacs to check if there's a Projectile project that the current directory belongs to, and if so, override the "current package" default.

Install the [projectile][] package for Emacs and do `M-x projectile-add-known-project <path to wire-server>`. Then add the following snippet to your `init.el`:

[projectile]: https://www.projectile.mx/en/latest/installation/

```
(require 'haskell)
(require 'projectile)

;; When inside a project, even if there is a cabal file in the current
;; folder, use the project folder to generate tags. This is useful for
;; projects with several services or subprojects.
(defadvice haskell-cabal--find-tags-dir (around project-tags act)
  (setq ad-return-value
    (if (projectile-project-root)
      (projectile-project-root)
      ad-do-it)))
```

### Haskell Language Server

To use HLS bundled in direnv setup, here is a sample `.dir-locals.el` that can
be put in the root directory of the project:
```
((haskell-mode . ((haskell-completion-backend . lsp)
                  (lsp-haskell-server-path . "/home/haskeller/code/wire-server/hack/bin/nix-hls.sh")
                  )))
```

### Ormolu integration

There are make targets `format`, `formatf`, `formatc` to re-format
or check the entire repository.  This takes about 10 seconds.

Emacs integration is [linked
here](https://github.com/tweag/ormolu#editor-integration).

## Vim {#DevVim}

### hie (Haskell IDE engine) integration

* Generate `hie.yaml` as described [at the top of this file](#using-haskell-ide-engine)
* You may follow the setup described [in this blog post](http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/)

### ormolu integration

There are make targets `format`, `formatf`, `formatc` to re-format
or check the entire repository.  This takes about 10 seconds.

Vim integration is [linked
here](https://github.com/tweag/ormolu#editor-integration).

If you use sdiehl's module, you may need to collect the language extensions from a cabal file:

```
    let g:ormolu_options = ["--ghc-opt -XAllowAmbiguousTypes --ghc-opt -XBangPatterns --ghc-opt -XConstraintKinds --ghc-opt -XDataKinds --ghc-opt -XDefaultSignatures --ghc-opt -XDerivingStrategies --ghc-opt -XDeriveFunctor --ghc-opt -XDeriveGeneric --ghc-opt -XDeriveLift --ghc-opt -XDeriveTraversable --ghc-opt -XDuplicateRecordFields --ghc-opt -XEmptyCase --ghc-opt -XFlexibleContexts --ghc-opt -XFlexibleInstances --ghc-opt -XFunctionalDependencies --ghc-opt -XGADTs --ghc-opt -XInstanceSigs --ghc-opt -XKindSignatures --ghc-opt -XLambdaCase --ghc-opt -XMultiParamTypeClasses --ghc-opt -XMultiWayIf --ghc-opt -XNamedFieldPuns --ghc-opt -XNoImplicitPrelude --ghc-opt -XOverloadedRecordDot --ghc-opt -XOverloadedStrings --ghc-opt -XPackageImports --ghc-opt -XPatternSynonyms --ghc-opt -XPolyKinds --ghc-opt -XQuasiQuotes --ghc-opt -XRankNTypes --ghc-opt -XScopedTypeVariables --ghc-opt -XStandaloneDeriving --ghc-opt -XTemplateHaskell --ghc-opt -XTupleSections --ghc-opt -XTypeApplications --ghc-opt -XTypeFamilies --ghc-opt -XTypeFamilyDependencies --ghc-opt -XTypeOperators --ghc-opt -XUndecidableInstances --ghc-opt -XViewPatterns"]
```

**EDIT:** this may no longer be necessary, as the ormolu version we
  use consults the cabal files for enabled language extensions.

## VSCode

The project can be loaded into the [Haskell Language Server](https://github.com/haskell/haskell-language-server).
This gives type checking, code completion, HLint hints, formatting with Ormolu, lookup of definitions and references, etc..
All needed dependencies (like the `haskell-language-server` and `stack` binaries) are provided by `shell.nix`.

Setup steps:
- Install the plugins `Haskell` (Haskell Language Server support), `Haskell Syntax` and `Nix Environment Selector`
- Generate the `hie.yaml` file: `make hie.yaml`
- Select the nix environment from `shell.nix` with the command `Nix-Env: Select environment`.
- Reload the window as proposed by the `Nix Environment Selector` plugin

An alternative way to make these dependencies accessible to VSCode is to start it in the `direnv` environment.
I.e. from a shell that's current working directory is in the project. The drawbacks of this approach are
that it only works locally (not on a remote connection) and one VSCode process needs to be started per project.

## Python

If you develop Python scripts in `hack/bin` and `./hack/python/wire/` make use
of `flake8`, `pylint` for linting and `black` for autoformatting.

If you are using emacs user linting can be configured by adding

```
(
 (python-mode . ((flycheck-python-flake8-executable . "/home/pythoneer/repos/wire-server/hack/bin/python3.sh")
                 (flycheck-python-pylint-executable . "/home/pythoneer/repos/wire-server/hack/bin/python3.sh")
                 (flycheck-python-pycompile-executable . "/home/pythoneer/repos/wire-server/hack/bin/python3.sh")
                 (python-shell-interpreter . "/home/pythoneer/repos/wire-server/hack/bin/python3.sh")
                 ))
)
```
to `.dir-locals.el` .
