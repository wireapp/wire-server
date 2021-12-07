{ mkDerivation, asn1-encoding, asn1-types, base, bytestring
, containers, cryptonite, data-default-class, hourglass, lib
, memory, mtl, pem, tasty, tasty-hunit, x509, x509-store
}:
mkDerivation {
  pname = "x509-validation";
  version = "1.6.11";
  sha256 = "f94321cbcc4a534adf5889ae6950f3673e38b62b89b6970b477f502ce987d19b";
  libraryHaskellDepends = [
    asn1-encoding asn1-types base bytestring containers cryptonite
    data-default-class hourglass memory mtl pem x509 x509-store
  ];
  testHaskellDepends = [
    asn1-encoding asn1-types base bytestring cryptonite
    data-default-class hourglass memory tasty tasty-hunit x509
    x509-store
  ];
  homepage = "http://github.com/vincenthz/hs-certificate";
  description = "X.509 Certificate and CRL validation";
  license = lib.licenses.bsd3;
}
