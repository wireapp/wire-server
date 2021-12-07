{ mkDerivation, base, binary, bytestring, cryptohash-md5
, data-binary-ieee754, lib, mtl, network, QuickCheck
, test-framework, test-framework-quickcheck2, text, time
}:
mkDerivation {
  pname = "bson";
  version = "0.4.0.1";
  sha256 = "6bc436f1671c19fbe3b56a52cf786a0f78f8a73a4b072af0aa006ce40286bdf6";
  libraryHaskellDepends = [
    base binary bytestring cryptohash-md5 data-binary-ieee754 mtl
    network text time
  ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2 text time
  ];
  homepage = "http://github.com/mongodb-haskell/bson";
  description = "BSON documents are JSON-like objects with a standard binary encoding";
  license = lib.licenses.asl20;
}
