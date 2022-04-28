{ mkDerivation, aeson, base, bytestring, cassandra-util, containers
, errors, exceptions, extra, hspec, hspec-discover, http-types
, imports, lib, metrics-wai, optparse-applicative, servant
, servant-server, servant-swagger, string-conversions, temporary
, tinylog, wai
}:
mkDerivation {
  pname = "extended";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra http-types imports metrics-wai optparse-applicative servant
    servant-server servant-swagger string-conversions tinylog wai
  ];
  testHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra hspec http-types imports metrics-wai optparse-applicative
    servant servant-server servant-swagger string-conversions temporary
    tinylog wai
  ];
  testToolDepends = [ hspec-discover ];
  description = "Extended versions of common modules";
  license = lib.licenses.agpl3Only;
}
