{ mkDerivation, attoparsec, base, base-compat, blaze-builder
, bytestring, containers, criterion, deepseq, deepseq-generics
, hedgehog, HUnit, lib, network-uri, safe, semigroups, tasty
, tasty-hedgehog, tasty-hunit, template-haskell, th-lift-instances
, transformers
}:
mkDerivation {
  pname = "uri-bytestring";
  version = "0.3.2.2";
  sha256 = "c7d47c25c1dbeae8c80ef7f83095093177e119effa90715a55b15fcaebd8ff6a";
  libraryHaskellDepends = [
    attoparsec base blaze-builder bytestring containers
    template-haskell th-lift-instances
  ];
  testHaskellDepends = [
    attoparsec base base-compat blaze-builder bytestring containers
    hedgehog HUnit safe semigroups tasty tasty-hedgehog tasty-hunit
    transformers
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring criterion deepseq deepseq-generics
    network-uri
  ];
  homepage = "https://github.com/Soostone/uri-bytestring";
  description = "Haskell URI parsing as ByteStrings";
  license = lib.licenses.bsd3;
}
