let
  pkgs = import ./nixpkgs.nix;
in
{
  inherit pkgs;

  env = pkgs.buildEnv {
    name = "dev-env";
    paths = [
      pkgs.awscli
      pkgs.jq
      pkgs.niv
      pkgs.zip
      pkgs.gnumake
      pkgs.entr

      (pkgs.python3.withPackages (ps: with ps; [ sphinx recommonmark rst2pdf sphinx-autobuild sphinxcontrib-fulltoc ]))
    ];
  };
}

