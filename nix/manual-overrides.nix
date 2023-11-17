{ libsodium, protobuf, hlib, mls-test-cli, fetchpatch, ... }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  binary-parsers = hlib.markUnbroken (hlib.doJailbreak hsuper.binary-parsers);
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  bytestring-conversion = hlib.markUnbroken (hsuper.bytestring-conversion);
  openapi3 = hlib.markUnbroken (hlib.dontCheck hsuper.openapi3);
  cql = hlib.appendPatch (hlib.markUnbroken hsuper.cql) (fetchpatch {
    url = "https://gitlab.com/twittner/cql/-/merge_requests/11.patch";
    sha256 = "sha256-qfcCRkKjSS1TEqPRVBU9Ox2DjsdGsYG/F3DrZ5JGoEI=";
  });
  ghc-source-gen = hlib.markUnbroken (hlib.doJailbreak hsuper.ghc-source-gen);
  proto-lens-protoc = hlib.doJailbreak hsuper.proto-lens-protoc;
  proto-lens-setup = hlib.doJailbreak hsuper.proto-lens-setup;
  hashtables = hsuper.hashtables_1_3;
  # in case everything breaks with the hashable update, use this 
  # hashable = hsuper.callHackage "hashable" "1.4.2.0" {};
  invertible = hlib.markUnbroken hsuper.invertible;
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  monoidal-containers = hlib.doJailbreak hsuper.monoidal-containers;
  network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
  one-liner = hlib.doJailbreak hsuper.one-liner;
  linear-generics = hsuper.linear-generics_0_2_2;
  polysemy = hlib.doJailbreak hsuper.polysemy;
  polysemy-check = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-check);
  polysemy-plugin = hlib.doJailbreak hsuper.polysemy-plugin;
  quickcheck-state-machine = hlib.dontCheck hsuper.quickcheck-state-machine;
  servant = hlib.doJailbreak hsuper.servant;
  servant-client = hlib.doJailbreak hsuper.servant-client;
  servant-client-core = hlib.doJailbreak hsuper.servant-client-core;
  servant-foreign = hlib.doJailbreak hsuper.servant-foreign;
  servant-multipart = hlib.doJailbreak hsuper.servant-multipart;
  servant-swagger-ui = hlib.doJailbreak hsuper.servant-swagger-ui;
  servant-swagger-ui-core = hlib.doJailbreak hsuper.servant-swagger-ui-core;
  singletons-th = hlib.doJailbreak hsuper.singletons-th;
  singletons-base = hlib.dontCheck (hlib.doJailbreak hsuper.singletons-base);
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  text-icu-translit = hlib.markUnbroken (hlib.dontCheck hsuper.text-icu-translit);
  text-short = hlib.dontCheck hsuper.text-short;
  template = hlib.markUnbroken (hlib.doJailbreak hsuper.template);
  type-errors = hlib.dontCheck hsuper.type-errors;
  th-abstraction = hsuper.th-abstraction_0_5_0_0;
  th-desugar = hlib.doJailbreak hsuper.th-desugar;
  wai-middleware-prometheus = hlib.doJailbreak hsuper.wai-middleware-prometheus;
  wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
  # transitive-anns has flaky tests
  transitive-anns = hlib.dontCheck hsuper.transitive-anns;
  http2-manager = hlib.enableCabalFlag hsuper.http2-manager "-f-test-trailing-dot";

  crypton-connection = hlib.markUnbroken hsuper.crypton-connection;
  # Patched dependency on crypton-connection
  HaskellNet-SSL = hsuper.HaskellNet-SSL;
  warp = hlib.dontCheck hsuper.warp;

  # PR with fix: https://github.com/freckle/hspec-junit-formatter/pull/23
  hspec-junit-formatter = hlib.markUnbroken (hlib.dontCheck hsuper.hspec-junit-formatter);

  # Some test seems to be broken
  hsaml2 = hlib.dontCheck hsuper.hsaml2;
  saml2-web-sso = hlib.dontCheck hsuper.saml2-web-sso;
  http2 = hlib.dontCheck hsuper.http2;


  # Disable tests because they need network access to a running cassandra
  #
  # Explicitly enable haddock because cabal2nix disables it for packages with
  # internal libraries
  cql-io = hlib.doHaddock (hlib.dontCheck hsuper.cql-io);
  amqp = hlib.dontCheck hsuper.amqp;

  # Needs network access to running ES
  # also the test suite doesn't compile https://github.com/NixOS/nixpkgs/pull/167957
  # due to related broken quickcheck-arbitrary-template
  bloodhound = hlib.dontCheck hsuper.bloodhound;

  # These tests require newer version on hspec-wai, which doesn't work with some of the wire-server packages.
  amazonka = hlib.doJailbreak (hlib.dontCheck hsuper.amazonka);
  amazonka-cloudfront = hlib.dontCheck hsuper.amazonka-cloudfront;
  amazonka-core = hlib.doJailbreak (hlib.dontCheck hsuper.amazonka-core);
  amazonka-dynamodb = hlib.dontCheck hsuper.amazonka-dynamodb;
  amazonka-s3 = hlib.dontCheck hsuper.amazonka-s3;
  amazonka-ses = hlib.dontCheck hsuper.amazonka-ses;
  amazonka-sns = hlib.dontCheck hsuper.amazonka-sns;
  amazonka-sqs = hlib.dontCheck hsuper.amazonka-sqs;
  amazonka-sso = hlib.dontCheck hsuper.amazonka-sso;
  amazonka-sts = hlib.dontCheck hsuper.amazonka-sts;
  servant-server = hlib.dontCheck hsuper.servant-server;

  # Build toool dependencies of local packages
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;

  # Make hoogle static to reduce size of the hoogle image
  hoogle = hlib.justStaticExecutables hsuper.hoogle;

  # Postie has been fixed upstream (master)
  postie = hlib.markUnbroken (hlib.doJailbreak hsuper.postie);

  # This would not be necessary if we could pull revision -r1 from 0.2.2.3
  kind-generics-th = hlib.doJailbreak hsuper.kind-generics-th;
}
