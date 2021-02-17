let
  sources = import ./sources.nix;
in
import sources.nixpkgs {
  overlays = [ (import ./wire-packages.nix) ] ++ ((import sources.haskell-nix) { }).overlays;
}
