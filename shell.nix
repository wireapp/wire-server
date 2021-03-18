{pkgs ? import ./nix/nixpkgs.nix }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    gnumake
    (python3.withPackages (ps: with ps; [ sphinx recommonmark awscli rst2pdf ]))
  ];
}
