let
  pkgs = import ./nix;
in pkgs.haskell.packages.ghc884.override { overrides = import ./nix/overlays/haskell.nix; }
