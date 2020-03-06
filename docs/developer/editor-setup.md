# Editor setup {#DevEditor}

This page provides tips for setting up editors to work with the Wire codebase.

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

### Ormolu integration

There are make targets `format`, `formatf`, `formatc` to re-format
or check the entire repository.  This takes about 10 seconds.

Emacs integration is [linked
here](https://github.com/tweag/ormolu#editor-integration).

## Vim {#DevVim}

TODO.

### ormolu integration

There are make targets `format`, `formatf`, `formatc` to re-format
or check the entire repository.  This takes about 10 seconds.

Vim integration is [linked
here](https://github.com/tweag/ormolu#editor-integration).

If you use sdiehl's module, you you need to collect the language extensions from `package-defaults.yaml`:

```
    let g:ormolu_options = ["--ghc-opt -XAllowAmbiguousTypes --ghc-opt -XBangPatterns --ghc-opt -XConstraintKinds --ghc-opt -XDataKinds --ghc-opt -XDefaultSignatures --ghc-opt -XDerivingStrategies --ghc-opt -XDeriveFunctor --ghc-opt -XDeriveGeneric --ghc-opt -XDeriveLift --ghc-opt -XDeriveTraversable --ghc-opt -XEmptyCase --ghc-opt -XFlexibleContexts --ghc-opt -XFlexibleInstances --ghc-opt -XFunctionalDependencies --ghc-opt -XGADTs --ghc-opt -XInstanceSigs --ghc-opt -XKindSignatures --ghc-opt -XLambdaCase --ghc-opt -XMultiParamTypeClasses --ghc-opt -XMultiWayIf --ghc-opt -XNamedFieldPuns --ghc-opt -XNoImplicitPrelude --ghc-opt -XOverloadedStrings --ghc-opt -XPackageImports --ghc-opt -XPatternSynonyms --ghc-opt -XPolyKinds --ghc-opt -XQuasiQuotes --ghc-opt -XRankNTypes --ghc-opt -XScopedTypeVariables --ghc-opt -XStandaloneDeriving --ghc-opt -XTemplateHaskell --ghc-opt -XTupleSections --ghc-opt -XTypeApplications --ghc-opt -XTypeFamilies --ghc-opt -XTypeFamilyDependencies --ghc-opt -XTypeOperators --ghc-opt -XUndecidableInstances --ghc-opt -XViewPatterns"]
```

If you want to be playful, you can look at how `tools/ormolu.sh`
collects the language extensions automatically and see if you can get
it to work here.
