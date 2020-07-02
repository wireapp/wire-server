{ pkgs ? import ./nix }:
with pkgs; mkShell {
  name = "shell";
  buildInputs = [
    docker-compose
    gnumake
    stack
    kind
    kubectl
  ];
}
