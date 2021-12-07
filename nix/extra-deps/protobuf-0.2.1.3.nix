{ mkDerivation, base, base-orphans, bytestring, cereal, containers
, data-binary-ieee754, deepseq, hex, HUnit, lib, mtl, QuickCheck
, tagged, tasty, tasty-hunit, tasty-quickcheck, text
, unordered-containers
}:
mkDerivation {
  pname = "protobuf";
  version = "0.2.1.3";
  sha256 = "a9fbff8d94e97a95ed8d959b5958e4633bd79d10df489764a36c160a91cb29f5";
  libraryHaskellDepends = [
    base base-orphans bytestring cereal data-binary-ieee754 deepseq mtl
    text unordered-containers
  ];
  testHaskellDepends = [
    base bytestring cereal containers hex HUnit mtl QuickCheck tagged
    tasty tasty-hunit tasty-quickcheck text unordered-containers
  ];
  homepage = "https://github.com/alphaHeavy/protobuf";
  description = "Google Protocol Buffers via GHC.Generics";
  license = lib.licenses.bsd3;
}
