{ mkDerivation, aeson, base, bytestring, errors, hpack, http-client
, http-types, imports, iso3166-country-codes, lib, text, time
}:
mkDerivation {
  pname = "ropes";
  version = "0.4.20";
  src = /home/axeman/workspace/wire-server/libs/ropes;
  libraryHaskellDepends = [
    aeson base bytestring errors http-client http-types imports
    iso3166-country-codes text time
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Various ropes to tie together with external web services";
  license = lib.licenses.agpl3Only;
}
