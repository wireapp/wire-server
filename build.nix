let pkgs = (import ./nix).pkgs;
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

      bonanza = ["bonanza"];
      billing-team-member-backfill = ["billing-team-member-backfill"];
      api-smoketest = ["api-smoketest"];
      zauth = ["zauth"];
    };

    attrsets = lib.attrsets;
    externalOverrides = import ./nix/haskell-overrides.nix;
    localOverrides = import ./nix/local-overrides.nix;
    manualOverrides = import ./nix/manual-overrides.nix (with pkgs; {
      inherit hlib libsodium protobuf snappy;
    });
    executableOverrides = hself: hsuper:
      attrsets.genAttrs (builtins.attrNames executablesMap) (e: withCleanedPath hsuper.${e});
    staticExecutableOverrides = hself: hsuper:
      attrsets.mapAttrs' (name: _:
        attrsets.nameValuePair "${name}-static" (hlib.justStaticExecutables hsuper."${name}")
      ) executablesMap;
    hPkgs = pkgs.haskell.packages.ghc8107.override{
      overrides = lib.composeManyExtensions [
        externalOverrides
        localOverrides
        manualOverrides
        executableOverrides
        staticExecutableOverrides
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

    imagesWithBrigTemplates = images // {brig = pkgs.dockerTools.buildImage {
      name = "quay.io/wire/brig";
      fromImage = images.brig;
      runAsRoot = ''
      #!${pkgs.runtimeShell}
      mkdir -p /usr/share/wire/
      ln -s ${brig-templates} /usr/share/wire/templates
      '';
    };};
in {
  images = imagesWithBrigTemplates;

  dev-shell = hPkgs.shellFor {
    packages = p: builtins.map (e: p.${e}) (builtins.attrNames (import ./nix/local-overrides.nix {} {}));
    buildInputs = [pkgs.cabal-install];
  };
  hPkgs = hPkgs;
} // attrsets.genAttrs (builtins.attrNames executablesMap) (e: hPkgs.${e})
