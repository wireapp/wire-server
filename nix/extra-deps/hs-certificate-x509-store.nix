{ mkDerivation, asn1-encoding, asn1-types, base, bytestring
, containers, cryptonite, directory, fetchgit, filepath, lib, mtl
, pem, tasty, tasty-hunit, x509
}:
mkDerivation {
  pname = "x509-store";
  version = "1.6.7";
  src = fetchgit {
    url = "https://github.com/vincenthz/hs-certificate";
    sha256 = "0ivc4l3c272i7w37rfgsbwnxa3fzfmghwddlqvzj5jj3zx5lyqlk";
    rev = "a899bda3d7666d25143be7be8f3105fc076703d9";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/x509-store; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    asn1-encoding asn1-types base bytestring containers cryptonite
    directory filepath mtl pem x509
  ];
  testHaskellDepends = [ base bytestring tasty tasty-hunit x509 ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "X.509 collection accessing and storing methods";
  license = lib.licenses.bsd3;
}
