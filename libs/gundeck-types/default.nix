{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, containers, imports, lens, lib
, network-uri, text, types-common, unordered-containers, wire-api
}:
mkDerivation {
  pname = "gundeck-types";
  version = "1.45.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring bytestring-conversion containers
    imports lens network-uri text types-common unordered-containers
    wire-api
  ];
  license = lib.licenses.agpl3Only;
}
