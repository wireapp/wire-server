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

  profileEnv = pkgs.writeTextFile {
    name = "profile-env";
    destination = "/.profile";
    # This gets sourced by direnv. Set NIX_PATH, so `nix-shell` uses the same nixpkgs as here.
    text = ''
      export NIX_PATH=nixpkgs=${toString pkgs.path}
    '';
  };

  devEnv = pkgs.buildEnv {
    name = "wire-server-direnv";
    paths = devPackages ++ [ profileEnv ];
  };
in
{
  inherit pkgs devPackages devEnv compile-deps;
}
