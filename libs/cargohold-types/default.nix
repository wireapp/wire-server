{ mkDerivation, base, bytestring-conversion, hpack, imports, lib
, types-common, wire-api
}:
mkDerivation {
  pname = "cargohold-types";
  version = "1.5.0";
  src = /home/axeman/workspace/wire-server/libs/cargohold-types;
  libraryHaskellDepends = [
    base bytestring-conversion imports types-common wire-api
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Asset Storage API Types";
  license = lib.licenses.agpl3Only;
}
