let pkgs = (import ./nix).pkgs;
    hlib = pkgs.haskell.lib;
    withCleanedPath = drv:
      hlib.overrideCabal drv (old: {
        src = pkgs.lib.cleanSourceWith {
          src = old.src;
          filter = path: type:
            let baseName = baseNameOf (toString path);
            in baseName != "dist";
        };
      });
    hPkgs = pkgs.haskell.packages.ghc8107.override {
      overrides = hself: hsuper:
        let externalOverrides = import ./nix/haskell-overrides.nix hself hsuper;
            localOverrides = import ./nix/local-overrides.nix hself hsuper;
            manualOverrides = {
              network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
              cql = hlib.markUnbroken hsuper.cql;
              cql-io =  hlib.markUnbroken (hlib.dontCheck hsuper.cql-io);
              lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
              wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
              bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
              invertible = hlib.markUnbroken hsuper.invertible;
              polysemy-check = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-check);
              swagger = hlib.doJailbreak externalOverrides.swagger;
              multihash = hlib.doJailbreak externalOverrides.multihash;
              wire-message-proto-lens = hlib.addBuildTool localOverrides.wire-message-proto-lens pkgs.protobuf;
              types-common-journal = hlib.addBuildTool localOverrides.types-common-journal pkgs.protobuf;
              hashable = hsuper.hashable_1_4_0_2;
              hashable-time = hsuper.hashable-time_0_3;
              time-compat = hsuper.time-compat_1_9_6_1;
              quickcheck-instances = hself.quickcheck-instances_0_3_27;
              text-short = pkgs.haskell.lib.dontCheck hsuper.text-short_0_1_5;
              aeson = hsuper.aeson_2_1_0_0;
              OneTuple = hsuper.OneTuple_0_3_1;
              semialign = hsuper.semialign_1_2_0_1;
              attoparsec = hsuper.attoparsec;
              swagger2 = hsuper.swagger2_2_8_2;
              servant-swagger-ui-core = hlib.doJailbreak hsuper.servant-swagger-ui-core;
              servant-swagger-ui = hlib.doJailbreak hsuper.servant-swagger-ui;
              sodium-crypto-sign = hlib.addPkgconfigDepend localOverrides.sodium-crypto-sign pkgs.libsodium.dev;
              lens = hsuper.lens_5_1;
              servant-foreign = hlib.doJailbreak hsuper.servant-foreign;
              servant-multipart = hlib.doJailbreak hsuper.servant-multipart;
              # singeltons = hlib.doJailbreak generated.singletons;
              # saml2-web-sso = hlib.dontCheck generated.saml2-web-sso;

              brig = withCleanedPath localOverrides.brig;
              cannon = withCleanedPath localOverrides.cannon;
              cargohold = withCleanedPath localOverrides.cargohold;
              federator = withCleanedPath localOverrides.federator;
              galley = withCleanedPath localOverrides.galley;
              gundeck = withCleanedPath localOverrides.gundeck;
              proxy = withCleanedPath localOverrides.proxy;
              spar = withCleanedPath localOverrides.spar;
            };
        in
          # builtins.mapAttrs (name: value: hlib.dontHaddock (hlib.dontCheck value))
          (externalOverrides // localOverrides // manualOverrides);
    };

in with hPkgs; rec {
  inherit brig cannon cargohold federator galley gundeck proxy spar;
}
