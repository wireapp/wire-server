{ mkDerivation, asn1-types, base, bytestring, hourglass, lib, mtl
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "asn1-encoding";
  version = "0.9.6";
  sha256 = "d9f8deabd3b908e5cf83c0d813c08dc0143b3ec1c0d97f660d2cfa02c1c8da0a";
  revision = "2";
  editedCabalFile = "16503ryhq15f2rfdav2qnkq11dg2r3vk3f9v64q9dmxf8dh8zv97";
  libraryHaskellDepends = [ asn1-types base bytestring hourglass ];
  testHaskellDepends = [
    asn1-types base bytestring hourglass mtl tasty tasty-quickcheck
  ];
  homepage = "https://github.com/vincenthz/hs-asn1";
  description = "ASN1 data reader and writer in RAW, BER and DER forms";
  license = lib.licenses.bsd3;
}
