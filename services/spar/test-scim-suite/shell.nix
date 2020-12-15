{ pkgs ? import (import ../../../nix/sources.nix).nixpkgs {} }:
with pkgs;
mkShell {
  buildInputs = [newman jq coreutils curl];
}
