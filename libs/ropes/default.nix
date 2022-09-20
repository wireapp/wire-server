{ mkDerivation, aeson, base, bytestring, errors, http-client
, http-types, imports, iso3166-country-codes, lib, text, time
}:
mkDerivation {
  pname = "ropes";
  version = "0.4.20";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring errors http-client http-types imports
    iso3166-country-codes text time
  ];
  description = "Various ropes to tie together with external web services";
  license = lib.licenses.agpl3Only;
}
