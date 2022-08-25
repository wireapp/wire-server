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
      api-smoketest = ["api-smoketest"];
      zauth = ["zauth"];
    };

    attrsets = lib.attrsets;

    pinnedPackages = import ./haskell-pins.nix {
      fetchgit = pkgs.fetchgit;
      inherit lib;
    };
    localPackages = import ./local-haskell-packages.nix;
    manualOverrides = import ./manual-overrides.nix (with pkgs; {
      inherit hlib libsodium protobuf snappy mls_test_cli;
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

    images = attrsets.mapAttrs (execName: drv:
      pkgs.dockerTools.buildImage {
        name = "quay.io/wire/${execName}";
        contents = [
          pkgs.cacert
          pkgs.coreutils
          pkgs.bashInteractive
          drv
        ];
      }
    ) staticExecs;

    brig-templates = pkgs.srcOnly {
      name = "brig-templates";
      src = ./services/brig/deb/opt/brig/templates;
    };

    imagesWithBrigTemplates = images // {
      brig = pkgs.dockerTools.buildImage {
        name = "quay.io/wire/brig";
        fromImage = images.brig;
        runAsRoot = ''
          #!${pkgs.runtimeShell}
          mkdir -p /usr/share/wire/
          ln -s ${brig-templates} /usr/share/wire/templates
          '';
      };
    };
    wireServerPackages = (builtins.attrNames (localPackages {} {}));
in {
  images = imagesWithBrigTemplates;

  devShell = hPkgs.shellFor {
    packages = p: builtins.map (e: p.${e}) wireServerPackages;
    buildInputs = [
      (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
      pkgs.cabal2nix
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

      pkgs.cabal-install
      pkgs.haskellPackages.cabal-plan
      pkgs.haskellPackages.cabal-fmt
      pkgs.nix-prefetch-git
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      # linux-only, not strictly required tools

      pkgs.docker-compose
      pkgs.telepresence
    ];
  };
  haskellPackages = hPkgs;
} // attrsets.genAttrs wireServerPackages (e: hPkgs.${e})
