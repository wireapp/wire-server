{ mkDerivation, aeson, attoparsec, attoparsec-iso8601, base
, base16-bytestring, base64-bytestring, binary, bytestring
, bytestring-conversion, cassandra-util, cereal, containers
, cryptohash-md5, cryptohash-sha1, cryptonite, data-default
, hashable, http-api-data, imports, iproute, lens, lens-datetime
, lib, optparse-applicative, protobuf, QuickCheck
, quickcheck-instances, random, schema-profunctor, scientific
, servant-server, singletons, string-conversions, swagger, swagger2
, tagged, tasty, tasty-hunit, tasty-quickcheck, text, time
, time-locale-compat, tinylog, unix, unordered-containers
, uri-bytestring, uuid, vector, yaml
}:
mkDerivation {
  pname = "types-common";
  version = "0.16.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base base16-bytestring
    base64-bytestring binary bytestring bytestring-conversion
    cassandra-util containers cryptohash-md5 cryptohash-sha1 cryptonite
    data-default hashable http-api-data imports iproute lens
    lens-datetime optparse-applicative protobuf QuickCheck
    quickcheck-instances random schema-profunctor scientific
    servant-server singletons string-conversions swagger swagger2
    tagged tasty text time time-locale-compat tinylog unix
    unordered-containers uri-bytestring uuid vector yaml
  ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    bytestring-conversion cereal imports protobuf QuickCheck
    string-conversions tasty tasty-hunit tasty-quickcheck text time
    unordered-containers uuid
  ];
  description = "Shared type definitions";
  license = lib.licenses.agpl3Only;
}
