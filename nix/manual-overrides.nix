{ libsodium, protobuf, snappy, hlib }:
hself: hsuper: {
  network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
  cql = hlib.markUnbroken hsuper.cql;
  cql-io =  hlib.markUnbroken (hlib.dontCheck hsuper.cql-io);
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  invertible = hlib.markUnbroken hsuper.invertible;
  polysemy-check = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-check);
  swagger = hlib.doJailbreak hsuper.swagger;
  multihash = hlib.doJailbreak hsuper.multihash;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  hashable = hsuper.hashable_1_4_0_2;
  hashable-time = hsuper.hashable-time_0_3;
  text-short = hlib.dontCheck hsuper.text-short;
  aeson = hsuper.aeson_2_1_0_0;
  lens-aeson = hsuper.lens-aeson_1_2_1;
  attoparsec = hsuper.attoparsec;
  swagger2 = hlib.doJailbreak hsuper.swagger2;
  servant-swagger-ui-core = hlib.doJailbreak hsuper.servant-swagger-ui-core;
  servant-swagger-ui = hlib.doJailbreak hsuper.servant-swagger-ui;
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  servant-foreign = hlib.doJailbreak hsuper.servant-foreign;
  servant-multipart = hlib.doJailbreak hsuper.servant-multipart;
  hashtables = hsuper.hashtables_1_3;
  quickcheck-state-machine = hlib.dontCheck hsuper.quickcheck-state-machine;
  amazonka-core = hlib.doJailbreak hsuper.amazonka-core;
  amazonka = hlib.doJailbreak hsuper.amazonka;
  wai-middleware-prometheus = hlib.doJailbreak hsuper.wai-middleware-prometheus;

  # Avoid infinite recursion
  snappy = hself.callPackage ./nix/haskell-overrides/snappy.nix { snappy = snappy; };
}
