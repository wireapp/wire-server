# This file provides nix attributes which can be used to build wire server
# components, the development shell, the CI Image and an image with hoogle.
#
# Most haskell dependencies come from the package set present in nixpkgs.
# However, the package set is not enough to build wire-server components and
# requires some tweaks. These tweaks are built using a few things:
#
# 1. nix/local-haskell-packages.nix: This file provides a list of overrides
# which add the local packages from this repository into the nixpkgs haskell
# package set. This file is generated using `make regen-local-nix-derivations`
# which uses cabal2nix to generate nix derivations for each haskell package in
# this repository.
#
# 2. nix/haskell-pins.nix: This file provides a list of overrides for haskell
# packages we wish to pin to a certain version, either because we have had to
# fork them or because we're using an old/new version of the package so it
# supports our use cases.
#
# 3. nix/manual-overrides.nix: This file provides a list of overrides that we
# have to manually maintain. This is different from pinned dependencies because
# it overrides packages in a few ways:
#
# 3.1. Broken packages: Some packages are marked broken in the nixpkgs package
# set, but they work if we use them with our pins or sometimes we have to
# disable their tests to make them work.
#
# 3.2: Version overrides: These are very similar to nix/haskell-pins.nix, but
# the package set itself sometimes contains newer versions of a few packages
# along with the old versions, e.g., the package set contains aeson and
# aeson_2_1_1_0. We use the latest version provided by the pacakge set, so we
# don't have to remember to update the version here, nixpkgs will take care of
# giving us the latest version.
#
# 3.3: External dependencies: cabal2nix sometimes fails to provide the external
# dependencies like adding protobuf and mls-test-cli as a buld tools. So, we
# need to write overrides to ensure these are present during build.
#
# 3.4: Other overrides: We may need to override haskell package derivations for
# some other reasons, like ensuring hoogle derivation produces just the
# executable. We can use nix/manual-overrides.nix for this.
#
# Using thse tweaks we can get a haskell package set which has wire-server
# components and the required dependencies. We then use this package set along
# with nixpkgs' dockerTools to make derivations for docker images that we need.
pkgs: pkgsCachix:
let
  lib = pkgs.lib;
  hlib = pkgs.haskell.lib;
  withCleanedPath = drv:
    hlib.overrideCabal drv (old: {
      src = lib.cleanSourceWith {
        src = old.src;
        filter = path: type:
          let baseName = baseNameOf (toString path);
          in baseName != "dist";
      };
    });

  gitignoreSource =
    let
      gitignoreSrc = pkgs.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        # put the latest commit sha of gitignore Nix library here:
        rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
        # use what nix suggests in the mismatch message here:
        sha256 = "sha256:07vg2i9va38zbld9abs9lzqblz193vc5wvqd6h7amkmwf66ljcgh";
      };
    in
    (import gitignoreSrc { inherit (pkgs) lib; }).gitignoreSource;

  # Mapping from package -> [executable]
  executablesMap = {
    brig = [ "brig" "brig-index" "brig-integration" "brig-schema" ];
    cannon = [ "cannon" ];
    cargohold = [ "cargohold" "cargohold-integration" ];
    federator = [ "federator" "federator-integration" ];
    galley = [ "galley" "galley-integration" "galley-schema" "galley-migrate-data" ];
    gundeck = [ "gundeck" "gundeck-integration" "gundeck-schema" ];
    proxy = [ "proxy" ];
    spar = [ "spar" "spar-integration" "spar-schema" "spar-migrate-data" ];
    stern = [ "stern" ];

    billing-team-member-backfill = [ "billing-team-member-backfill" ];
    inconsistencies = [ "inconsistencies" ];
    api-simulations = [ "api-smoketest" "api-loadtest" ];
    zauth = [ "zauth" ];
  };

  attrsets = lib.attrsets;

  pinnedPackages = import ./haskell-pins.nix {
    fetchgit = pkgs.fetchgit;
    inherit lib;
  };

  localPackages = { enableOptimization, enableDocs, enableTests }: hsuper: hself:
    # The default packages are expected to have optimizations and docs turned
    # on.
    let
      defaultPkgs = import ./local-haskell-packages.nix
        {
          inherit gitignoreSource;
        }
        hsuper
        hself;

      werror = _: hlib.failOnAllWarnings;
      opt = _: drv:
        if enableOptimization
        then drv
        else
        # We need to explicitly add `-O0` because all the cabal files
        # explicitly have `-O2` in them
          hlib.appendConfigureFlag (hlib.disableOptimization drv) "--ghc-option=-O0";
      tests = _: drv:
        if enableTests
        then drv
        else hlib.dontCheck drv;
      docs = _: drv:
        if enableDocs
        then drv
        else hlib.dontHaddock drv;

      overrideAll = fn: overrides:
        attrsets.mapAttrs fn (overrides);
    in
    lib.lists.foldr overrideAll defaultPkgs [
      werror
      opt
      docs
      tests
    ];
  manualOverrides = import ./manual-overrides.nix (with pkgs; {
    inherit hlib libsodium protobuf mls-test-cli;
  });

  executables = hself: hsuper:
    attrsets.genAttrs (builtins.attrNames executablesMap) (e: withCleanedPath hsuper.${e});

  staticExecutables = hself: hsuper:
    attrsets.mapAttrs'
      (name: _:
        attrsets.nameValuePair "${name}-static" (hlib.justStaticExecutables hsuper."${name}")
      )
      executablesMap;

  hPkgs = localMods@{ enableOptimization, enableDocs, enableTests }: pkgs.haskell.packages.ghc92.override {
    overrides = lib.composeManyExtensions [
      pinnedPackages
      (localPackages localMods)
      manualOverrides
      executables
      staticExecutables
    ];
  };

  extractExec = localMods@{ enableOptimization, enableDocs, enableTests }: hPkgName: execName:
    pkgs.stdenv.mkDerivation {
      name = execName;
      buildInputs = [ (hPkgs localMods)."${hPkgName}-static" ];
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out/bin
        cp "${(hPkgs localMods)."${hPkgName}-static"}/bin/${execName}" "$out/bin/${execName}"
      '';
    };

  # We extract static executables out of the output of building the packages
  # so they don't depend on all the haskell dependencies. These exectuables
  # are "static" from the perspective of ghc, i.e. they don't dynamically
  # depend on other haskell packages but they still dynamically depend on C
  # dependencies like openssl, cryptobox, libxml2, etc. Doing this makes the
  # final images that we generate much smaller as we don't have to carry
  # around so files for all haskell packages.
  staticExecs = localMods@{ enableOptimization, enableDocs, enableTests }:
    let
      nested = attrsets.mapAttrs
        (hPkgName: execNames:
          attrsets.genAttrs execNames (extractExec localMods hPkgName)
        )
        executablesMap;
      unnested = lib.lists.foldr (x: y: x // y) { } (attrsets.attrValues nested);
    in
    unnested;

  # Docker tools doesn't create tmp directories but some processes need this
  # and so we have to create it ourself.
  tmpDir = pkgs.runCommand "tmp-dir" { } ''
    mkdir -p $out/tmp
    mkdir -p $out/var/tmp
  '';

  brig-templates = pkgs.stdenvNoCC.mkDerivation {
    name = "brig-templates";
    src = ../services/brig/deb/opt/brig/templates;
    installPhase = ''
      mkdir -p $out/usr/share/wire
      cp -r $src $out/usr/share/wire/templates
    '';
  };

  # Some images require extra things which is not possible to specify using
  # cabal file dependencies, so cabal2nix cannot automatically add these.
  #
  # extraContents :: Map Text [Derivation]
  extraContents = {
    brig = [ brig-templates ];
    brig-integration = [ brig-templates pkgs.mls-test-cli ];
    galley-integration = [ pkgs.mls-test-cli ];
  };

  # useful to poke around a container during a 'kubectl exec'
  debugUtils = with pkgs; [
    bashInteractive
    gnugrep
    coreutils
    dig
    curl
    less
    gnutar
    gzip
    openssl
    which
  ];

  images = localMods@{ enableOptimization, enableDocs, enableTests }:
    attrsets.mapAttrs
      (execName: drv:
        pkgs.dockerTools.streamLayeredImage {
          name = "quay.io/wire/${execName}";
          maxLayers = 10;
          contents = [
            pkgs.cacert
            pkgs.iana-etc
            pkgs.dumb-init
            drv
            tmpDir
          ] ++ debugUtils ++ pkgs.lib.optionals (builtins.hasAttr execName extraContents) (builtins.getAttr execName extraContents);
          # Any mkdir running in this step won't actually make it to the image,
          # hence we use the tmpDir derivation in the contents
          fakeRootCommands = ''
            chmod 1777 tmp
            chmod 1777 var/tmp
          '';
          config = {
            Entrypoint = [ "${pkgs.dumb-init}/bin/dumb-init" "--" "${drv}/bin/${execName}" ];
            Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" ];
          };
        }
      )
      (staticExecs localMods);

  localModsEnableAll = {
    enableOptimization = true;
    enableDocs = true;
    enableTests = true;
  };
  localModsOnlyTests = {
    enableOptimization = false;
    enableDocs = false;
    enableTests = true;
  };
  localModsOnlyDocs = {
    enableOptimization = false;
    enableDocs = true;
    enableTests = false;
  };

  imagesList = pkgs.writeTextFile {
    name = "imagesList";
    text = "${lib.concatStringsSep "\n" (builtins.attrNames (images localModsEnableAll))}";
  };
  wireServerPackages = (builtins.attrNames (localPackages localModsEnableAll { } { }));

  hoogle = (hPkgs localModsOnlyDocs).hoogleWithPackages (p: builtins.map (e: p.${e}) wireServerPackages);

  # More about dockerTools.streamLayeredImage:
  # https://nixos.org/manual/nixpkgs/unstable/#ssec-pkgs-dockerTools-streamLayeredImage
  hoogleImage = pkgs.dockerTools.streamLayeredImage {
    name = "quay.io/wire/wire-server-hoogle";
    maxLayers = 50;
    contents = [
      pkgs.cacert
      pkgs.coreutils
      pkgs.bashInteractive
      pkgs.dumb-init
      hoogle
    ];
    config = {
      Entrypoint = [ "${pkgs.dumb-init}/bin/dumb-init" "--" "${hoogle}/bin/hoogle" "server" "--local" "--host=*" ];
      Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" ];
    };
  };

  # Tools common between CI and developers
  commonTools = [
    pkgs.cabal2nix
    pkgs.gnumake
    pkgs.gnused
    pkgs.parallel
    pkgs.ripgrep
    pkgs.helm
    pkgs.helmfile
    pkgs.hlint
    (hlib.justStaticExecutables pkgs.haskellPackages.apply-refact)
    pkgs.jq
    pkgs.kubectl
    pkgs.nixpkgs-fmt
    pkgs.ormolu
    pkgs.shellcheck
    pkgs.treefmt
    pkgs.gawk
    pkgs.cfssl
    (hlib.justStaticExecutables pkgs.haskellPackages.cabal-fmt)
  ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
    pkgs.skopeo
  ];

  # Building an image which can do nix builds is hard. This is programmed
  # nicely in docker.nix at the root of https://github.com/nixos/nix. We get
  # this file using "${pkgs.nix.src}/docker.nix" so we don't have to also pin
  # the nix repository along with the nixpkgs repository.
  ciImage = import "${pkgs.nix.src}/docker.nix" {
    inherit pkgs;
    name = "quay.io/wire/wire-server-ci";
    maxLayers = 2;
    # We don't need to push the "latest" tag, every step in CI should depend
    # deterministically on a specific image.
    tag = null;
    bundleNixpkgs = false;
    # FUTUREWORK: Use pkgs.cachix here when we update `nixpkgs` to latest nixpkgs-unstable
    extraPkgs = commonTools ++ [ pkgsCachix.cachix ];
    nixConf = {
      experimental-features = "nix-command";
    };
  };

  shell = (hPkgs localModsOnlyTests).shellFor {
    packages = p: builtins.map (e: p.${e}) wireServerPackages;
  };
  ghcWithPackages = shell.nativeBuildInputs ++ shell.buildInputs;

  profileEnv = pkgs.writeTextFile {
    name = "profile-env";
    destination = "/.profile";
    # This gets sourced by direnv. Set NIX_PATH, so `nix-shell` uses the same nixpkgs as here.
    text = ''
      export NIX_PATH=nixpkgs=${toString pkgs.path}
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
    '';
  };
in
{
  inherit ciImage hoogleImage;

  images = images localModsEnableAll;
  imagesUnoptimizedNoDocs = images localModsOnlyTests;
  # Used for production images, ensure that optimizations and tests are always
  # enabled!
  imagesNoDocs = images {
    enableOptimization = true;
    enableTests = true;
    enableDocs = false;
  };
  inherit imagesList;

  devEnv = pkgs.buildEnv {
    name = "wire-server-dev-env";
    paths = commonTools ++ [
      (pkgs.haskell-language-server.override { supportedGhcVersions = [ "92" ]; })
      pkgs.ghcid
      pkgs.kind
      pkgs.netcat
      pkgs.niv
      (pkgs.python3.withPackages
        (ps: with ps; [ pyyaml requests ]))
      pkgs.rsync
      pkgs.wget
      pkgs.yq
      pkgs.nginz

      pkgs.cabal-install
      pkgs.haskellPackages.cabal-plan
      pkgs.nix-prefetch-git
      profileEnv
    ]
    ++ ghcWithPackages
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      # linux-only, not strictly required tools
      pkgs.docker-compose
      pkgs.telepresence
    ];
  };

  inherit brig-templates;
  haskellPackages = hPkgs localModsEnableAll;
  haskellPackagesUnoptimizedNoDocs = hPkgs localModsOnlyTests;
} // attrsets.genAttrs (wireServerPackages) (e: hPkgs.${e})
