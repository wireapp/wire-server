{ mkDerivation, base, lib }:
mkDerivation {
  pname = "indexed-profunctors";
  version = "0.1";
  sha256 = "31dfb4319dff84199344000b1efad75158eeac17ddcbb27f91735e958591bb65";
  libraryHaskellDepends = [ base ];
  description = "Utilities for indexed profunctors";
  license = lib.licenses.bsd3;
}
