{ libsodium, protobuf, hlib, mls-test-cli, fetchurl, curl, pkg-config, postgresql, openssl, icu, stdenv, ... }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  # ----------------
  # tests don't pass
  # (these are in general not fine they need to be investigated)
  # FUTUREWORK: investigate whether all of these tests need to fail
  # ----------------

  # tests don't work, but only in a flake
  saml2-web-sso = hlib.dontCheck hsuper.saml2-web-sso;

  # test suite doesn't compile and needs network access
  bloodhound = hlib.dontCheck hsuper.bloodhound;

  # tests need network access, cabal2nix disables haddocks
  cql-io = hlib.doHaddock (hlib.dontCheck hsuper.cql-io);

  quickcheck-state-machine = hlib.markUnbroken (hlib.dontCheck hsuper.quickcheck-state-machine);

  # Tests require a running redis
  hedis = hlib.dontCheck hsuper.hedis;

  HaskellNet = hlib.dontCheck hsuper.HaskellNet;

  # Tests require a running postgresql
  hasql = hlib.dontCheck hsuper.hasql;
  hasql-pool = hlib.dontCheck hsuper.hasql-pool;
  hasql-migration = hlib.markUnbroken (hlib.dontCheck hsuper.hasql-migration);
  hasql-transaction = hlib.dontCheck hsuper.hasql-transaction; # users 1.2.1 from nixpkgs
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
  lrucaching = hlib.doJailbreak (hlib.markUnbroken hsuper.lrucaching);
  # added servant-openapi3 because the version bounds of some dependent packages
  # of our pin exclude the versions in our current nixpkgs
  servant-openapi3 = hlib.doJailbreak (hlib.dontCheck hsuper.servant-openapi3);

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

  # hs-opentelemetry pin removal bumps API -> 0.3.0.0 and SDK -> 0.1.0.1 from the pinned commit; instrumentation stays at 0.1.1.0/0.1.0.1.
  hs-opentelemetry-instrumentation-wai = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-wai);
  hs-opentelemetry-instrumentation-conduit = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-conduit);
  hs-opentelemetry-instrumentation-http-client = hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-http-client;
  hs-opentelemetry-utils-exceptions = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-utils-exceptions);

  # ------------------------------------
  # okay but marked broken (nixpkgs bug)
  # (we can unfortunately not do anything here but update nixpkgs)
  # ------------------------------------
  template = hlib.markUnbroken hsuper.template;
  system-linux-proc = hlib.markUnbroken hsuper.system-linux-proc;
  # FSEvents doesn't work in nix sandbox on macOS; on Linux inotify works fine
  fsnotify = (if stdenv.isDarwin then hlib.dontCheck else (x: x))
    (hlib.markUnbroken hsuper.fsnotify);

  # Federator monitor tests use fsnotify which doesn't work in nix sandbox on macOS
  federator = (if stdenv.isDarwin then hlib.dontCheck else (x: x)) hsuper.federator;

  # -----------------
  # version overrides
  # (these are fine but will probably need to be adjusted in a future nixpkgs update)
  # -----------------
  # warp requires curl in its testsuite
  warp = hlib.addTestToolDepends hsuper.warp [ curl ];

  http-semantics = hsuper.http-semantics_0_4_0;
  network-run = hsuper.network-run_0_5_0;
  http2 = hsuper.http2_5_4_0;

  # -----------------
  # flags and patches
  # (these are fine)
  # -----------------
  cryptostore = hlib.addBuildDepends (hlib.dontCheck (hlib.appendConfigureFlags hsuper.cryptostore [ "-fuse_crypton" ]))
    [ hself.crypton hself.crypton-x509 hself.crypton-x509-validation ];
  # doJailbreak because upstreams requires a specific crypton-connection version we don't have
  hoogle = hlib.justStaticExecutables (hlib.dontCheck (hsuper.hoogle));

  # Extra dependencies/flags for local packages
  http2-manager = hlib.disableCabalFlag hsuper.http2-manager "test-trailing-dot";
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  text-icu-translit = hlib.addPkgconfigDepend hsuper.text-icu-translit icu;
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
