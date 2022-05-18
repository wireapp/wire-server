{ pkgs ? (import ../../../nix).pkgs }:
with pkgs;
mkShell {
  buildInputs = [newman jq coreutils curl];
}
