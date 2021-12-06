{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, base64-bytestring, bytestring, bytestring-conversion
, cassandra-util, cereal, containers, cryptohash-md5
, cryptohash-sha1, cryptonite, data-default, hashable, hpack
, imports, iproute, lens, lens-datetime, lib, optparse-applicative
, protobuf, QuickCheck, quickcheck-instances, random
, schema-profunctor, scientific, servant-server, singletons
, string-conversions, swagger, swagger2, tagged, tasty, tasty-hunit
, tasty-quickcheck, text, time, time-locale-compat, tinylog, unix
, unordered-containers, uri-bytestring, uuid, vector, yaml
}:
mkDerivation {
  pname = "types-common";
  version = "0.16.0";
  src = /home/axeman/workspace/wire-server/libs/types-common;
  libraryHaskellDepends = [
    aeson attoparsec base base16-bytestring base64-bytestring
    bytestring bytestring-conversion cassandra-util containers
    cryptohash-md5 cryptohash-sha1 cryptonite data-default hashable
    imports iproute lens lens-datetime optparse-applicative protobuf
    QuickCheck quickcheck-instances random schema-profunctor scientific
    servant-server singletons string-conversions swagger swagger2
    tagged tasty text time time-locale-compat tinylog unix
    unordered-containers uri-bytestring uuid vector yaml
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    bytestring-conversion cereal imports protobuf QuickCheck
    string-conversions tasty tasty-hunit tasty-quickcheck text time
    unordered-containers uuid
  ];
  prePatch = "hpack";
  description = "Shared type definitions";
  license = lib.licenses.agpl3Only;
}
