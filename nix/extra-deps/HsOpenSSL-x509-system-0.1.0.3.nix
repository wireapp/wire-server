{ mkDerivation, base, bytestring, HsOpenSSL, lib, unix }:
mkDerivation {
  pname = "HsOpenSSL-x509-system";
  version = "0.1.0.3";
  sha256 = "5bdcb7ae2faba07a374109fea0a1431ae09d080f8574e60ab7a351b46f931f92";
  libraryHaskellDepends = [ base bytestring HsOpenSSL unix ];
  homepage = "https://github.com/redneb/HsOpenSSL-x509-system";
  description = "Use the system's native CA certificate store with HsOpenSSL";
  license = lib.licenses.bsd3;
}
