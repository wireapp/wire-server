{ mkDerivation, aeson, async, base, bilge, bytestring
, bytestring-conversion, data-timeout, exceptions, http-client
, http-types, imports, lib, random, tasty-hunit, types-common
, websockets, wire-api
}:
mkDerivation {
  pname = "tasty-cannon";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bilge bytestring bytestring-conversion
    data-timeout exceptions http-client http-types imports random
    tasty-hunit types-common websockets wire-api
  ];
  description = "Cannon Integration Testing Utilities";
  license = lib.licenses.agpl3Only;
}
