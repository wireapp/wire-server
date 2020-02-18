let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [
      # the tool we use for versioning (The thing that generates sources.json)
      (_: _: { niv = (import sources.niv {}).niv; })
      (import ./overlays/server.nix)
      (import ./overlays/wire-server.nix)

    ]
    # The toolset that converts stack projects to nix projects
    ++ (import (sources."haskell.nix" + /overlays));
  };
in
  pkgs

