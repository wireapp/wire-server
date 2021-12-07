{ mkDerivation, base, bytestring, hourglass, lib, memory }:
mkDerivation {
  pname = "asn1-types";
  version = "0.3.4";
  sha256 = "78ee92a251379298ca820fa53edbf4b33c539b9fcd887c86f520c30e3b4e21a8";
  libraryHaskellDepends = [ base bytestring hourglass memory ];
  homepage = "http://github.com/vincenthz/hs-asn1";
  description = "ASN.1 types";
  license = lib.licenses.bsd3;
}
