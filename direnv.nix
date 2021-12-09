{ pkgs ? import ./nix }:
let
  packages = import ./dev-packages.nix { pkgs = pkgs; };
in
pkgs.buildEnv {
  name = "wire-server-direnv";
  paths = packages;
}
