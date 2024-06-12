{ libsodium, protobuf, hlib, mls-test-cli, ... }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  # ----------------
  # tests don't pass
  # (these are in general not fine they need to be investigated)
  # FUTUREWORK: investigate whether all of these tests need to fail
  # ----------------
  amqp = hlib.dontCheck hsuper.amqp_0_22_2;
  # test suite doesn't compile and needs network access
  bloodhound = hlib.dontCheck hsuper.bloodhound;
  # tests need network access, cabal2nix disables haddocks
  cql-io = hlib.doHaddock (hlib.dontCheck hsuper.cql-io);
  # PR with fix: https://github.com/freckle/hspec-junit-formatter/pull/23
  # the PR has been merged, but has not arrived in nixpkgs
  hspec-junit-formatter = hlib.markUnbroken (hlib.dontCheck hsuper.hspec-junit-formatter);
  markov-chain-usage-model = hlib.markUnbroken (hlib.dontCheck hsuper.markov-chain-usage-model);
  openapi3 = hlib.markUnbroken (hlib.dontCheck hsuper.openapi3);
  quickcheck-state-machine = hlib.dontCheck hsuper.quickcheck-state-machine;
  saml2-web-sso = hlib.dontCheck hsuper.saml2-web-sso;
  # one of the tests is flaky
  transitive-anns = hlib.dontCheck hsuper.transitive-anns;
  warp = hlib.dontCheck hsuper.warp;

  # Tests require a running redis
  hedis = hlib.dontCheck hsuper.hedis;

  # ---------------------
  # need to be jailbroken
  # (these need to be fixed upstream eventually)
  # FUTUREWORK: fix the dependency bounds upstream
  # ---------------------
  binary-parsers = hlib.markUnbroken (hlib.doJailbreak hsuper.binary-parsers);
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
  proto-lens-protoc = hlib.doJailbreak hsuper.proto-lens-protoc;
  proto-lens-setup = hlib.doJailbreak hsuper.proto-lens-setup;
  th-desugar = hlib.doJailbreak hsuper.th-desugar;

  # ------------------------------------
  # okay but marked broken (nixpkgs bug)
  # (we can unfortunately not do anything here but update nixpkgs)
  # ------------------------------------
  bytestring-conversion = hlib.markUnbroken hsuper.bytestring-conversion;
  template = hlib.markUnbroken hsuper.template;
  polysemy-test = hlib.markUnbroken hsuper.polysemy-test;

  # -----------------
  # version overrides
  # (these are fine but will probably need to be adjusted in a future nixpkgs update)
  # -----------------
  hpack = hsuper.hpack_0_36_0;
  linear-generics = hsuper.linear-generics_0_2_2;
  network-conduit-tls = hsuper.network-conduit-tls_1_4_0;
  optparse-generic = hsuper.optparse-generic_1_5_2;
  th-abstraction = hsuper.th-abstraction_0_5_0_0;
  tls = hsuper.tls_1_9_0;
  warp-tls = hsuper.warp-tls_3_4_3;

  # -----------------
  # flags and patches
  # (these are fine)
  # -----------------
  cryptostore = hlib.addBuildDepends (hlib.dontCheck (hlib.appendConfigureFlags hsuper.cryptostore [ "-fuse_crypton" ]))
    [ hself.crypton hself.crypton-x509 hself.crypton-x509-validation ];
  # Make hoogle static to reduce size of the hoogle image
  hoogle = hlib.justStaticExecutables hsuper.hoogle;
  http2-manager = hlib.enableCabalFlag hsuper.http2-manager "-f-test-trailing-dot";
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
}
