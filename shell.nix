{ pkgs ? import <nixpkgs> {}}:
with pkgs; mkShell {
  name = "shell";
  buildInputs = [ 
    docker-compose
    gnumake
    stack
  ];
}
