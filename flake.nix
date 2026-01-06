{
  description = "A very basic flake";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs_24_11.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, nixpkgs_24_11, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/overlay.nix)
            (import ./nix/overlay-docs.nix)
          ];
        };
        pkgs_24_11 = import nixpkgs_24_11 {
          inherit system;
        };
        wireServerPkgs = import ./nix { inherit pkgs pkgs_24_11; };
      in
      {
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
