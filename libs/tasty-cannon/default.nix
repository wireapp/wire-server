{ mkDerivation, aeson, async, base, bilge, bytestring
, bytestring-conversion, data-timeout, exceptions, gundeck-types
, hpack, http-client, http-types, imports, lib, random, tasty-hunit
, types-common, websockets
}:
mkDerivation {
  pname = "tasty-cannon";
  version = "0.4.0";
  src = /home/axeman/workspace/wire-server/libs/tasty-cannon;
  libraryHaskellDepends = [
    aeson async base bilge bytestring bytestring-conversion
    data-timeout exceptions gundeck-types http-client http-types
    imports random tasty-hunit types-common websockets
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Cannon Integration Testing Utilities";
  license = lib.licenses.agpl3Only;
}
