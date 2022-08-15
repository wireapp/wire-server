let
  sources = import ./sources.nix;

  pkgs = import sources.nixpkgs {
    config.allowUnfree = true;
    overlays = [
      # All wire-server specific packages
      (import ./overlay.nix)
    ];
  };

  # a different pin for building wire-server docs.
  # should eventually be moved to use the same pin.
  pkgsDocs = import sources.nixpkgsDocs {
    overlays = [
      (import ./overlay-docs.nix)
    ];
  };

  c-lib-out-deps = [
    pkgs.cryptobox
    pkgs.icu.out
    pkgs.libsodium.out
    pkgs.libxml2.out
    pkgs.ncurses.out
    pkgs.openssl.out
    pkgs.pcre.out
    pkgs.snappy.out
    pkgs.zlib.out
    pkgs.lzma.out
  ];

  compile-deps = pkgs.buildEnv {
    name = "wire-server-compile-deps";
    paths = [
      pkgs.stdenv.cc.cc.lib

      pkgs.bash
      pkgs.coreutils
      pkgs.gnused
      pkgs.gnugrep
      pkgs.pkgconfig
      pkgs.gawk
      pkgs.git

      pkgs.haskell.compiler.ghc8107
      pkgs.protobuf

      pkgs.cryptobox
      pkgs.icu.dev
      pkgs.libsodium.dev
      pkgs.libxml2.dev
      pkgs.ncurses.dev
      pkgs.openssl.dev
      pkgs.pcre.dev
      pkgs.snappy.dev
      pkgs.zlib.dev
      pkgs.lzma.dev
    ] ++ c-lib-out-deps;
  };

  # This performs roughly the same setup as direnv's load_prefix function, but
  # only when invoking cabal. This means that we can set LD_LIBRARY_PATH just
  # for cabal, as setting it in direnv can interfere with programs in the host
  # system, especially for non-NixOS users.
  cabal-wrapper = pkgs.writeShellScriptBin "cabal" ''
    export NIX_BUILD_SHELL="${pkgs.bash}/bin/bash"
    export CPATH="${compile-deps}/include"
    export LD_LIBRARY_PATH="${compile-deps}/lib"
    export LIBRARY_PATH="${compile-deps}/lib"
    export PKG_CONFIG_PATH="${compile-deps}/lib/pkgconfig"
    export PATH="${compile-deps}/bin"
    export CONFIG_SHELL="${compile-deps}/bin/sh"
    exec "${pkgs.cabal-install}/bin/cabal" "$@"
  '';

  # stack-deps.nix sets LD_LIBRARY_PATH, which could be incompatible with the
  # system bash. To ensure that nix-shell invoked by stack uses the correct
  # shell to build we set NIX_BUILD_SHELL here.
  stack-wrapper = pkgs.writeShellScriptBin "stack" ''
    export NIX_BUILD_SHELL="${pkgs.bash}/bin/bash"
    exec "${pkgs.stack}/bin/stack" "$@"
  '';

  devPackages = [
    (pkgs.haskell-language-server.override {
      dynamic = true;
      supportedGhcVersions = [ "8107" ];
    })
    pkgs.cfssl
    pkgs.mls_test_cli
    pkgs.ghcid
    pkgs.gnumake
    pkgs.gnused
    pkgs.helm
    pkgs.helmfile
    pkgs.hlint
    pkgs.jq
    pkgs.kind
    pkgs.kubectl
    pkgs.netcat
    pkgs.niv
    pkgs.ormolu
    pkgs.shellcheck
    pkgs.python3
    pkgs.rsync
    pkgs.shellcheck
    pkgs.wget
    pkgs.yq

    cabal-wrapper
    stack-wrapper

    pkgs.haskellPackages.cabal-plan
    pkgs.haskellPackages.cabal-fmt

    # We don't use pkgs.cabal-install here, as we invoke it with a wrapper
    # which sets LD_LIBRARY_PATH and others correctly.
    pkgs.haskellPackages.implicit-hie

  ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
    # linux-only, not strictly required tools

    pkgs.docker-compose
    pkgs.telepresence
    pkgs.buildah # To actually run buildah on nixos, I had to follow this: https://gist.github.com/alexhrescale/474d55635154e6b2cd6362c3bb403faf
  ]
  ++ c-lib-out-deps; # Required to run HLS

  profileEnv = pkgs.writeTextFile {
    name = "profile-env";
    destination = "/.profile";
    # This gets sourced by direnv. Set NIX_PATH, so `nix-shell` uses the same nixpkgs as here.
    text = ''
      export NIX_PATH=nixpkgs=${toString pkgs.path}
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
    '';
  };

  devEnv = pkgs.buildEnv {
    name = "wire-server-direnv";
    paths = devPackages ++ [ profileEnv ];
  };

  # packages necessary to build wire-server docs
  docsPkgs = [
    pkgsDocs.texlive.combined.scheme-full
    (pkgsDocs.python3.withPackages
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
    pkgs.runCommandNoCC
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
        pkgsDocs.awscli
        pkgsDocs.jq
        pkgsDocs.niv
        pkgsDocs.zip
        pkgsDocs.entr
      ] ++ docsPkgs;
    };
in
{
  inherit pkgs devPackages devEnv docs docsEnv compile-deps;
}
