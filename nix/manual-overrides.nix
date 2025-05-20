{ libsodium, protobuf, hlib, mls-test-cli, fetchurl, curl, ... }:
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

  # ---------------------
  # need to be jailbroken
  # (these need to be fixed upstream eventually)
  # FUTUREWORK: fix the dependency bounds upstream
  # ---------------------
  binary-parsers = hlib.markUnbroken (hlib.doJailbreak hsuper.binary-parsers);
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  postie = hlib.doJailbreak hsuper.postie;

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
  crypton-connection = hsuper.crypton-connection_0_4_4; # older version doesn't allow tls 2.1

  # warp requires curl in its testsuite
  warp = hlib.addTestToolDepends hsuper.warp [ curl ];

  # cabal multirepl requires Cabal 3.12
  Cabal = hsuper.Cabal_3_12_1_0;
  Cabal-syntax = hsuper.Cabal-syntax_3_12_1_0;

  # -----------------
  # flags and patches
  # (these are fine)
  # -----------------
  cryptostore = hlib.addBuildDepends (hlib.dontCheck (hlib.appendConfigureFlags hsuper.cryptostore [ "-fuse_crypton" ]))
    [ hself.crypton hself.crypton-x509 hself.crypton-x509-validation ];
  # doJailbreak because upstreams requires a specific crypton-connection version we don't have
  hoogle = hlib.justStaticExecutables (hlib.doJailbreak (hlib.dontCheck (hsuper.hoogle)));
  http2-manager = hlib.enableCabalFlag hsuper.http2-manager "-f-test-trailing-dot";
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
}
