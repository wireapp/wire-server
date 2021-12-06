{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, containers, hpack, imports, lens, lib
, network-uri, text, types-common, unordered-containers, wire-api
}:
mkDerivation {
  pname = "gundeck-types";
  version = "1.45.0";
  src = /home/axeman/workspace/wire-server/libs/gundeck-types;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring bytestring-conversion containers
    imports lens network-uri text types-common unordered-containers
    wire-api
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
