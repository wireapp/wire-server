{ mkDerivation, base, binary, bytestring, criterion, cryptohash-md5
, cryptohash-sha1, entropy, HUnit, lib, mersenne-random-pure64
, network-info, QuickCheck, random, tasty, tasty-hunit
, tasty-quickcheck, text, time, uuid-types
}:
mkDerivation {
  pname = "uuid";
  version = "1.3.13";
  sha256 = "dfac808a7026217d018b408eab18facc6a85c6183be308d4ac7877e80599b027";
  revision = "6";
  editedCabalFile = "06w8i9hi9ha84nmj6fcj44apzv61m3ryc0a1yxxqq5n8vznk2iya";
  libraryHaskellDepends = [
    base binary bytestring cryptohash-md5 cryptohash-sha1 entropy
    network-info random text time uuid-types
  ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck random tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base criterion mersenne-random-pure64 random
  ];
  homepage = "https://github.com/hvr/uuid";
  description = "For creating, comparing, parsing and printing Universally Unique Identifiers";
  license = lib.licenses.bsd3;
}
