{ libsodium, protobuf, hlib, mls-test-cli, fetchurl, curl, pkg-config, postgresql, openssl, ... }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  # ----------------
  # tests don't pass
  # (these are in general not fine they need to be investigated)
  # FUTUREWORK: investigate whether all of these tests need to fail
  # ----------------

  # test suite doesn't compile and needs network access
  bloodhound = hlib.dontCheck hsuper.bloodhound;

  # tests need network access, cabal2nix disables haddocks
  cql-io = hlib.doHaddock (hlib.dontCheck hsuper.cql-io);

  quickcheck-state-machine = hlib.dontCheck hsuper.quickcheck-state-machine;

  # Tests require a running redis
  hedis = hlib.dontCheck (hlib.doJailbreak hsuper.hedis);

  HaskellNet = hlib.dontCheck hsuper.HaskellNet;
  singletons-base-code-generator = hlib.markUnbroken (hlib.doJailbreak (hlib.dontCheck hsuper.singletons-base-code-generator));

  # Tests require a running postgresql
  hasql = hlib.dontCheck hsuper.hasql;
  hasql-pool = hlib.dontCheck hsuper.hasql-pool;
  hasql-migration = hlib.markUnbroken (hlib.dontCheck hsuper.hasql-migration);
  hasql-transaction = hlib.dontCheck hsuper.hasql-transaction_1_2_0_1;
  postgresql-binary = hlib.dontCheck (hsuper.postgresql-binary);

  # ---------------------
  # need to be jailbroken
  # (these need to be fixed upstream eventually)
  # FUTUREWORK: fix the dependency bounds upstream
  # ---------------------
  binary-parsers = hlib.markUnbroken (hlib.doJailbreak hsuper.binary-parsers);
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  postie = hlib.doJailbreak hsuper.postie;
  polysemy-time = hlib.doJailbreak (hsuper.polysemy-time);
  polysemy-resume = hlib.doJailbreak (hsuper.polysemy-resume);
  polysemy-conc = hlib.doJailbreak (hsuper.polysemy-conc);
  text-builder-core = hlib.doJailbreak hsuper.text-builder-core;
  text-builder = hlib.doJailbreak hsuper.text-builder;
  bitvec = hlib.doJailbreak hsuper.bitvec;

  # the libsodium haskell library is incompatible with the new version of the libsodium c library
  # that nixpkgs has - this downgrades libsodium from 1.0.19 to 1.0.18
  libsodium = hlib.markUnbroken (hlib.addPkgconfigDepend hsuper.libsodium (
    libsodium.overrideAttrs (old:
      rec {
        # we don't care for the patches for mingw and for 1.0.19
        patches = [ ];
        version = "1.0.18";
        src = fetchurl {
          url = "https://download.libsodium.org/libsodium/releases/${old.pname}-${version}.tar.gz";
          hash = "sha256-b1BEkLNCpPikxKAvybhmy++GItXfTlRStGvhIeRmNsE=";
        };
      }
    )));

  # ------------------------------------
  # okay but marked broken (nixpkgs bug)
  # (we can unfortunately not do anything here but update nixpkgs)
  # ------------------------------------
  template = hlib.markUnbroken hsuper.template;
  system-linux-proc = hlib.markUnbroken hsuper.system-linux-proc;
  lrucaching = hlib.markUnbroken hsuper.lrucaching;

  # -----------------
  # version overrides
  # (these are fine but will probably need to be adjusted in a future nixpkgs update)
  # -----------------
  # warp requires curl in its testsuite
  warp = hlib.addTestToolDepends hsuper.warp [ curl ];

  # cabal multirepl requires Cabal 3.12
  Cabal = hsuper.Cabal_3_12_1_0;
  Cabal-syntax = hsuper.Cabal-syntax_3_14_2_0;

  #  # 1.16 requires a too old template-haskell for GHC 9.10
  th-desugar = hsuper.th-desugar_1_18;
  #  # 3.3 requires a too old template-haskell for GHC 9.10
  singletons-th = (hlib.doJailbreak hsuper.singletons-th_3_5);
  proto-lens = (hlib.doJailbreak hsuper.proto-lens);
  # requires a too old base and template-haskell for GHC 9.10
  openapi3 = (hlib.doJailbreak hsuper.openapi3);
  # requires a too old base for GHC 9.10
  servant-openapi3 = (hlib.doJailbreak hsuper.servant-openapi3);
  singletons-base = hlib.dontCheck (hlib.doJailbreak hsuper.singletons-base_3_5);

  # requires a too old base for GHC 9.10
  amazonka-core = (hlib.doJailbreak hsuper.amazonka-core);
  # requires a too old base for GHC 9.10
  tasty-wai = (hlib.doJailbreak hsuper.tasty-wai);
  # -----------------
  # flags and patches
  # (these are fine)
  # -----------------
  cryptostore = hlib.addBuildDepends (hlib.dontCheck (hlib.appendConfigureFlags hsuper.cryptostore [ "-fuse_crypton" ]))
    [ hself.crypton hself.crypton-x509 hself.crypton-x509-validation ];
  # doJailbreak because upstreams requires a specific crypton-connection version we don't have
  hoogle = hlib.justStaticExecutables (hlib.doJailbreak (hlib.dontCheck (hsuper.hoogle)));

  # Extra dependencies/flags for local packages
  http2-manager = hlib.enableCabalFlag hsuper.http2-manager "-f-test-trailing-dot";
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
  postgresql-libpq-pkgconfig = hlib.addBuildDepends
    (hlib.markUnbroken hsuper.postgresql-libpq-pkgconfig)
    [ pkg-config postgresql.dev openssl.dev ];
  postgresql-libpq = hlib.overrideCabal
    (hlib.enableCabalFlag hsuper.postgresql-libpq "use-pkg-config")
    (drv: {
      libraryHaskellDepends = with hself; [
        base
        bytestring
        postgresql-libpq-pkgconfig
        unix
      ];
    });
}
