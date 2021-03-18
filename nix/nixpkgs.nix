let sources = import ./sources.nix;

in
import sources.nixpkgs {
  config = { };
  overlays = [
    (import ./overlay.nix)
  ];
}
