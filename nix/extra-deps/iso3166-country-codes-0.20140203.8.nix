{ mkDerivation, base, lib }:
mkDerivation {
  pname = "iso3166-country-codes";
  version = "0.20140203.8";
  sha256 = "b4d6e01cd61bcaef9a8e455c331a8e7a2298531cb587ef6f23675eae7a6b0a36";
  revision = "1";
  editedCabalFile = "0n01pmvkqi0w9l203i1v7kb6bb867plv4h5hmzlkpnhrf5abf0zf";
  libraryHaskellDepends = [ base ];
  description = "A datatype for ISO 3166 country codes";
  license = "LGPL";
}
