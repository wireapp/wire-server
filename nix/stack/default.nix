{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz
, nixpkgs ? haskellNixSrc + "/nixpkgs" }:

let
  pkgs = import nixpkgs (import haskellNixSrc);

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
