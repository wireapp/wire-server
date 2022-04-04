let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [
      # All wire-server specific packages
      (import ./overlay.nix)

    ];
  };
in
pkgs
