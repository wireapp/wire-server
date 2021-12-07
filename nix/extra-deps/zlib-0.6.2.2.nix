{ mkDerivation, base, bytestring, lib, QuickCheck, tasty
, tasty-hunit, tasty-quickcheck, zlib
}:
mkDerivation {
  pname = "zlib";
  version = "0.6.2.2";
  sha256 = "04b5890dd69e992f8cd09570d81e9d5ecab19db8e82cbe47ba8e02c31c0631ba";
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ zlib ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  description = "Compression and decompression in the gzip and zlib formats";
  license = lib.licenses.bsd3;
}
