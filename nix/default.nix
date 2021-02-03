let
  sources = import ./sources.nix;
in
import sources.nixpkgs {
  overlays = map import [
    ./wire-packages.nix
  ];
};
