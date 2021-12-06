{ mkDerivation, aeson, base, bytestring, cassandra-util, containers
, errors, exceptions, extra, hpack, hspec, hspec-discover
, http-types, imports, lib, metrics-wai, optparse-applicative
, servant, servant-server, servant-swagger, string-conversions
, temporary, tinylog, wai
}:
mkDerivation {
  pname = "extended";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/extended;
  libraryHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra http-types imports metrics-wai optparse-applicative servant
    servant-server servant-swagger string-conversions tinylog wai
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra hspec http-types imports metrics-wai optparse-applicative
    servant servant-server servant-swagger string-conversions temporary
    tinylog wai
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  description = "Extended versions of common modules";
  license = lib.licenses.agpl3Only;
}
