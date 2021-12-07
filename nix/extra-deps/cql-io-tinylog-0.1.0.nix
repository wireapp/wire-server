{ mkDerivation, base, bytestring, cql-io, lib, tinylog }:
mkDerivation {
  pname = "cql-io-tinylog";
  version = "0.1.0";
  sha256 = "0e8429fb27cac7a9da54b13a1de77facd780bf8f02a82eac2b0206f34e0cb992";
  libraryHaskellDepends = [ base bytestring cql-io tinylog ];
  homepage = "https://gitlab.com/romanb/cql-io-tinylog";
  description = "Tinylog integration for cql-io";
  license = lib.licenses.publicDomain;
}
