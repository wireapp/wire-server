{ libsodium, protobuf, hlib, mls-test-cli }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
  cql = hlib.markUnbroken hsuper.cql;
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  invertible = hlib.markUnbroken hsuper.invertible;
  polysemy-check = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-check);
  swagger = hlib.doJailbreak hsuper.swagger;
  multihash = hlib.markUnbroken (hlib.doJailbreak hsuper.multihash);
  hashable = hsuper.hashable_1_4_1_0;
  hashable-time = hsuper.hashable-time_0_3;
  text-short = hlib.dontCheck hsuper.text-short;
  aeson = hsuper.aeson_2_1_1_0;
  lens-aeson = hsuper.lens-aeson_1_2_2;
  swagger2 = hlib.doJailbreak hsuper.swagger2;
  servant-swagger-ui-core = hlib.doJailbreak hsuper.servant-swagger-ui-core;
  servant-swagger-ui = hlib.doJailbreak hsuper.servant-swagger-ui;
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  servant-foreign = hlib.doJailbreak hsuper.servant-foreign;
  servant-multipart = hlib.doJailbreak hsuper.servant-multipart;
  hashtables = hsuper.hashtables_1_3;
  quickcheck-state-machine = hlib.dontCheck hsuper.quickcheck-state-machine;
  wai-middleware-prometheus = hlib.doJailbreak hsuper.wai-middleware-prometheus;
  one-liner = hlib.doJailbreak hsuper.one-liner;
  type-errors = hlib.dontCheck hsuper.type-errors;
  binary-parsers = hlib.doJailbreak hsuper.binary-parsers;
  generic-data = hsuper.generic-data_1_0_0_0;

  # Some test seems to be broken
  hsaml2 = hlib.dontCheck hsuper.hsaml2;
  saml2-web-sso = hlib.dontCheck hsuper.saml2-web-sso;

  # Needs network access to a running cassandra
  cql-io = hlib.dontCheck hsuper.cql-io;

  # Needs network access to running ES
  # also the test suite doesn't compile https://github.com/NixOS/nixpkgs/pull/167957
  # due to related broken quickcheck-arbitrary-template
  bloodhound = hlib.dontCheck hsuper.bloodhound;

  # These tests require newer version on hspec-wai, which doesn't work with some of the wire-server packages.
  amazonka = hlib.doJailbreak (hlib.dontCheck hsuper.amazonka);
  amazonka-core = hlib.doJailbreak (hlib.dontCheck hsuper.amazonka-core);
  amazonka-cloudfront = hlib.dontCheck hsuper.amazonka-cloudfront;
  amazonka-dynamodb = hlib.dontCheck hsuper.amazonka-dynamodb;
  amazonka-s3 = hlib.dontCheck hsuper.amazonka-s3;
  amazonka-ses = hlib.dontCheck hsuper.amazonka-ses;
  amazonka-sns = hlib.dontCheck hsuper.amazonka-sns;
  amazonka-sqs = hlib.dontCheck hsuper.amazonka-sqs;
  amazonka-sso = hlib.dontCheck hsuper.amazonka-sso;
  amazonka-sts = hlib.dontCheck hsuper.amazonka-sts;
  servant-server = hlib.dontCheck hsuper.servant-server;

  # Build toool dependencies of local packages
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;

  # Make hoogle static to reduce size of the hoogle image
  hoogle = hlib.justStaticExecutables hsuper.hoogle;

  # Postie has been fixed upstream (master)
  postie = hlib.markUnbroken (hlib.doJailbreak hsuper.postie);

  # This would not be necessary if we could pull revision -r1 from 0.2.2.3
  kind-generics-th = hlib.doJailbreak hsuper.kind-generics-th;
}
