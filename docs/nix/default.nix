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
      pkgs.texlive.combined.scheme-full

      (pkgs.python3.withPackages (ps: with ps; [ sphinx recommonmark rst2pdf sphinx-autobuild sphinxcontrib-fulltoc sphinxcontrib-kroki sphinx-multiversion sphinx_rtd_theme ]))
    ];
  };
}

