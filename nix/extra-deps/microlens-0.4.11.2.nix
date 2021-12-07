{ mkDerivation, base, lib }:
mkDerivation {
  pname = "microlens";
  version = "0.4.11.2";
  sha256 = "4e484d4a73c7c5176ccfdacc29aec7399352cac1c7e8924d5123857cf36ddffc";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/monadfix/microlens";
  description = "A tiny lens library with no dependencies";
  license = lib.licenses.bsd3;
}
