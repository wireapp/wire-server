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
  nginz = pkgs.callPackage ./nginz.nix { };
  nginz-disco = pkgs.callPackage ./nginz-disco.nix { };

  # packages necessary to build wire-server docs
  docsPkgs = [
    pkgs.plantuml
    pkgs.texlive.combined.scheme-full
    (pkgs.python310.withPackages
      (ps: with ps; [
        myst-parser
        rst2pdf
        sphinx
        sphinx-autobuild
        sphinx-multiversion
        sphinx_rtd_theme
        sphinx_reredirects
        sphinx-copybutton
        sphinxcontrib-fulltoc
        sphinxcontrib-kroki
        sphinxcontrib-plantuml
      ]))
  ];

  docs =
    pkgs.runCommand
      "wire-docs"
      {
        nativeBuildInputs = docsPkgs ++ [ pkgs.gnumake ];
      }
      ''
        mkdir docs charts services
        cp -rH ${pkgs.nix-gitignore.gitignoreSource [] ../docs}/* docs/
        # GrepInclude snippets in the docs refer to files under ../charts/ and ../services/, 
        # so we need to copy these too before building.
        # FUTUREWORK: perhaps there is a nicer way to copy everything that does not need 3 separate lines,
        # however the statement `../` inside `cp -rH $#{pkgs.nix-gitignore.gitignoreSource [] ../}* .` is not valid.
        cp -rH ${pkgs.nix-gitignore.gitignoreSource [] ../charts}/* charts/
        cp -rH ${../services}/* services/
        chmod -R +w ./docs/src
        cp ${../CHANGELOG.md} ./docs/src/changelog/changelog.md
        cd docs
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
  inherit (pkgs) mls-test-cli;
in
{ inherit pkgs profileEnv wireServer docs docsEnv mls-test-cli nginz nginz-disco; }
