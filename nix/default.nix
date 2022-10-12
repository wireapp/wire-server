let
  sources = import ./sources.nix;

  pkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [
      # All wire-server specific packages
      (import ./overlay.nix)
      (import ./overlay-docs.nix)
    ];
  };

  profileEnv = pkgs.writeTextFile {
      name = "profile-env";
      destination = "/.profile";
      # This gets sourced by direnv. Set NIX_PATH, so `nix-shell` uses the same nixpkgs as here.
      text = ''
        export NIX_PATH=nixpkgs=${toString pkgs.path}
        export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
        '';
    };

  wireServer = import ./wire-server.nix pkgs;

  # packages necessary to build wire-server docs
  docsPkgs = [
    pkgs.texlive.combined.scheme-full
    (pkgs.python3.withPackages
      (ps: with ps; [
        myst-parser
        rst2pdf
        sphinx
        sphinx-autobuild
        sphinx-multiversion
        sphinx_rtd_theme
        sphinxcontrib-fulltoc
        sphinxcontrib-kroki
      ]))
  ];

  docs =
    pkgs.runCommand
      "wire-docs"
      {
        nativeBuildInputs = docsPkgs ++ [ pkgs.gnumake ];
      }
      ''
        cp -r ${pkgs.nix-gitignore.gitignoreSource [] ../docs}/* .
        make docs-all
        mkdir $out
        cp -r build/* $out/
      '';

  docsEnv = pkgs.buildEnv
    {
      name = "wire-server-docs-env";
      paths = [
        pkgs.awscli
        pkgs.jq
        pkgs.niv
        pkgs.zip
        pkgs.entr
      ] ++ docsPkgs;
    };
  mls-test-cli = pkgs.mls-test-cli;
  rusty-jwt-tools = pkgs.rusty-jwt-tools;
in {inherit pkgs profileEnv wireServer docs docsEnv mls-test-cli;}
