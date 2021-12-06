{ mkDerivation, base, bytestring, fetchgit, lib, QuickCheck, snappy
, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "snappy";
  version = "0.2.0.2";
  src = fetchgit {
    url = "https://github.com/wireapp/snappy";
    sha256 = "1d1x0kkh2p4mb29wi31wpffgr64i27jd10ci70i81y5fwn44c542";
    rev = "b0e5c08af48911caecffa4fa6a3e74872018b258";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ snappy ];
  testHaskellDepends = [
    base bytestring QuickCheck test-framework
    test-framework-quickcheck2
  ];
  homepage = "http://github.com/bos/snappy";
  description = "Bindings to the Google Snappy library for fast compression/decompression";
  license = lib.licenses.bsd3;
}
