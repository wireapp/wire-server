{ mkDerivation, base, containers, lib, transformers }:
mkDerivation {
  pname = "lens-family-core";
  version = "2.0.0";
  sha256 = "19b4fcd3bd37dd0056c112a9b16cf405644fabd6652013c61a5078380ed2265a";
  libraryHaskellDepends = [ base containers transformers ];
  description = "Haskell 2022 Lens Families";
  license = lib.licenses.bsd3;
}
