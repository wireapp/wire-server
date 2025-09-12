{
  description = "A very basic flake";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (import ./nix/overlay.nix)
              (import ./nix/overlay-docs.nix)
            ];
          };
          wireServerPkgs = import ./nix { inherit pkgs; };
      in {
        # profileEnv wireServer docs docsEnv mls-test-cli nginz;
        packages = {
          inherit (wireServerPkgs) pkgs profileEnv wireServer docs docsEnv mls-test-cli nginz;
        };
        devShells = {
          default = wireServerPkgs.wireServer.devEnv;
        };
      }
    );
}
