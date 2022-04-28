{ mkDerivation, base, bytestring-conversion, imports, lib
, types-common, wire-api
}:
mkDerivation {
  pname = "cargohold-types";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring-conversion imports types-common wire-api
  ];
  description = "Asset Storage API Types";
  license = lib.licenses.agpl3Only;
}
