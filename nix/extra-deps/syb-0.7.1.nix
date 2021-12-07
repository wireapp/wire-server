{ mkDerivation, base, containers, HUnit, lib, mtl }:
mkDerivation {
  pname = "syb";
  version = "0.7.1";
  sha256 = "e04a9c7ae48d9fe25e9b7a064f7122d39c710753544511e15df7a6e87fdfe700";
  revision = "1";
  editedCabalFile = "0rgxzwnbwawi8visnpq74s51n0qi9rzgnxsm2bdmi4vwfn3lb6w0";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base containers HUnit mtl ];
  homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
  description = "Scrap Your Boilerplate";
  license = lib.licenses.bsd3;
}
