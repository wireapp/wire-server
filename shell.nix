{ pkgs ? import <nixpkgs> {}}:
with pkgs; mkShell {
  name = "shell";
  buildInputs = [
    docker-compose
    gnumake
    stack
    cargo
    libsodium
    pcre
    openssl
    zlib
  ];
}
