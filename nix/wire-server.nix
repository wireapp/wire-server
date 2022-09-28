pkgs:
let lib = pkgs.lib;
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
      let gitignoreSrc = pkgs.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        # put the latest commit sha of gitignore Nix library here:
        rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
        # use what nix suggests in the mismatch message here:
        sha256 = "sha256:07vg2i9va38zbld9abs9lzqblz193vc5wvqd6h7amkmwf66ljcgh";
      };
      in (import gitignoreSrc { inherit (pkgs) lib; }).gitignoreSource;

    # Mapping from package -> executbale
    executablesMap = {
      brig = ["brig" "brig-index" "brig-integration" "brig-schema"];
      cannon = ["cannon"];
      cargohold = ["cargohold" "cargohold-integration"];
      federator = ["federator" "federator-integration"];
      galley = ["galley" "galley-integration" "galley-schema" "galley-migrate-data"];
      gundeck = ["gundeck" "gundeck-integration" "gundeck-schema"];
      proxy = ["proxy"];
      spar = ["spar" "spar-integration" "spar-schema" "spar-migrate-data"];
      stern = ["stern"];

      billing-team-member-backfill = ["billing-team-member-backfill"];
      api-simulations = ["api-smoketest" "api-loadtest"];
      zauth = ["zauth"];
    };

    attrsets = lib.attrsets;

    pinnedPackages = import ./haskell-pins.nix {
      fetchgit = pkgs.fetchgit;
      inherit lib;
    };
    localPackages = import ./local-haskell-packages.nix {
      inherit gitignoreSource;
    };
    manualOverrides = import ./manual-overrides.nix (with pkgs; {
      inherit hlib libsodium protobuf snappy mls-test-cli;
    });

    executables = hself: hsuper:
      attrsets.genAttrs (builtins.attrNames executablesMap) (e: withCleanedPath hsuper.${e});

    staticExecutables = hself: hsuper:
      attrsets.mapAttrs' (name: _:
        attrsets.nameValuePair "${name}-static" (hlib.justStaticExecutables hsuper."${name}")
      ) executablesMap;

    hPkgs = pkgs.haskell.packages.ghc8107.override{
      overrides = lib.composeManyExtensions [
        pinnedPackages
        localPackages
        manualOverrides
        executables
        staticExecutables
      ];
    };

    extractExec = hPkgName: execName:
      pkgs.stdenv.mkDerivation {
        name = execName;
        buildInputs = [hPkgs."${hPkgName}-static"];
        phases = "installPhase";
        installPhase = ''
          mkdir -p $out/bin
          cp "${hPkgs."${hPkgName}-static"}/bin/${execName}" "$out/bin/${execName}"
          '';
      };

    staticExecs =
      let nested = attrsets.mapAttrs (hPkgName: execNames:
            attrsets.genAttrs execNames (extractExec hPkgName)
          ) executablesMap;
          unnested = lib.lists.foldr (x: y: x // y) {} (attrsets.attrValues nested);
      in unnested;

    tmpDir = pkgs.runCommand "tmp-dir" {} ''
       mkdir -p $out/tmp
       mkdir -p $out/var/tmp
    '';

    images = attrsets.mapAttrs (execName: drv:
      pkgs.dockerTools.buildLayeredImage {
        name = "quay.io/wire/${execName}";
        maxLayers = 10;
        contents = [
          pkgs.cacert
          pkgs.iana-etc
          pkgs.coreutils
          pkgs.bashInteractive
          pkgs.dumb-init
          drv
          tmpDir
        ];
        fakeRootCommands = ''
          chmod 1777 tmp
          chmod 1777 var/tmp
        '';
        config = {
          Entrypoint = ["${pkgs.dumb-init}/bin/dumb-init" "--" "${drv}/bin/${execName}"];
        };
      }
    ) staticExecs;

    brig-templates = pkgs.stdenvNoCC.mkDerivation {
      name = "brig-templates";
      src = ../services/brig/deb/opt/brig/templates;
      installPhase = ''
         mkdir -p $out/usr/share/wire
         cp -r $src $out/usr/share/wire/templates
      '';
    };

    imagesWithPatches = images // {
      brig = pkgs.dockerTools.buildImage {
        name = "quay.io/wire/brig";
        fromImage = images.brig;
        copyToRoot = pkgs.buildEnv {
          name = "brig-templates";
          paths = [brig-templates];
        };
        config = {
          Entrypoint = ["${pkgs.dumb-init}/bin/dumb-init" "--" "${staticExecs.brig}/bin/brig"];
        };
      };
      brig-integration = pkgs.dockerTools.buildImage {
        name = "quay.io/wire/brig-integration";
        fromImage = images.brig-integration;
        copyToRoot = pkgs.buildEnv {
          name = "mls-test-cli";
          paths = [pkgs.mls-test-cli];
        };
        config = {
          Entrypoint = ["${pkgs.dumb-init}/bin/dumb-init" "--" "${staticExecs.brig-integration}/bin/brig-integration"];
        };
      };
      galley-integration = pkgs.dockerTools.buildImage {
        name = "quay.io/wire/galley-integration";
        fromImage = images.galley-integration;
        copyToRoot = pkgs.buildEnv {
          name = "mls-test-cli";
          paths = [pkgs.mls-test-cli];
        };
        config = {
          Entrypoint = ["${pkgs.dumb-init}/bin/dumb-init" "--" "${staticExecs.galley-integration}/bin/galley-integration"];
        };
      };
    };
    imagesList = pkgs.writeTextFile { name = "imagesList"; text = "${lib.concatStringsSep "\n" (builtins.attrNames imagesWithPatches)}"; };
    wireServerPackages = (builtins.attrNames (localPackages {} {}));

    # Tools common between CI and developers
    commonTools =  [
      pkgs.cabal2nix
      pkgs.gnumake
      pkgs.gnused
      pkgs.helm
      pkgs.helmfile
      pkgs.hlint
      pkgs.jq
      pkgs.kubectl
      pkgs.ormolu
      pkgs.shellcheck
      (hlib.justStaticExecutables pkgs.haskellPackages.cabal-fmt)
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      pkgs.skopeo
    ];

    sources = import ./sources.nix;
    ciImage = import "${sources.nix}/docker.nix" {
      inherit pkgs;
      name = "quay.io/wire/wire-server-ci";
      maxLayers = 2;
      # We don't need to push the "latest" tag, every step in CI should depend
      # deterministically on a specific image.
      tag = null;
      bundleNixpkgs = false;
      extraPkgs = commonTools ++ [pkgs.cachix];
    };
in {
  inherit ciImage;

  images = imagesWithPatches;
  imagesList = imagesList;

  devShell = hPkgs.shellFor {
    packages = p: builtins.map (e: p.${e}) wireServerPackages;
    buildInputs = commonTools ++ [
      (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
      pkgs.ghcid
      pkgs.cfssl
      pkgs.kind
      pkgs.netcat
      pkgs.niv
      pkgs.python3
      pkgs.rsync
      pkgs.wget
      pkgs.yq

      pkgs.cabal-install
      pkgs.haskellPackages.cabal-plan
      pkgs.nix-prefetch-git
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      # linux-only, not strictly required tools

      pkgs.docker-compose
      pkgs.telepresence
    ];
  };
  inherit brig-templates;
  haskellPackages = hPkgs;
} // attrsets.genAttrs (wireServerPackages) (e: hPkgs.${e})
