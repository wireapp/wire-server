let
  sources = import ./sources.nix;

  pkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [
      # All wire-server specific packages
      (import ./overlay.nix)
    ];
  };

  devPackages = (import ./dev-packages.nix { inherit pkgs; }).devPackages;

  compile-deps = (import ./dev-packages.nix { inherit pkgs; }).compile-deps;

  devEnv = pkgs.buildEnv {
    name = "wire-server-direnv";
    paths = devPackages;
  };
in
{
  inherit pkgs devPackages devEnv compile-deps;
}
