{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    gnumake
    (python3.withPackages (ps: with ps; [ sphinx recommonmark awscli ]))
  ];
}
