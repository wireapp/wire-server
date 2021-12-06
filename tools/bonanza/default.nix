{ mkDerivation, aeson, async, attoparsec, base, binary, bytestring
, cereal, cereal-conduit, clock, conduit, conduit-extra, containers
, cryptonite, errors, exceptions, geoip2, hpack, http-client
, http-client-tls, http-conduit, http-types, imports, iproute, lens
, lens-aeson, lib, network, network-bsd, optparse-applicative
, protobuf, QuickCheck, safe, scientific, snappy, snappy-framing
, stm, tasty, tasty-quickcheck, text, time, unordered-containers
, vector
}:
mkDerivation {
  pname = "bonanza";
  version = "3.6.0";
  src = /home/axeman/workspace/wire-server/tools/bonanza;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base binary bytestring cereal cereal-conduit
    conduit conduit-extra containers exceptions geoip2 http-client
    http-types imports iproute lens lens-aeson network network-bsd
    optparse-applicative protobuf safe scientific snappy snappy-framing
    text time unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async base bytestring clock conduit conduit-extra containers
    cryptonite errors http-client http-client-tls http-conduit imports
    optparse-applicative stm text time
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring conduit conduit-extra http-types
    imports lens QuickCheck scientific tasty tasty-quickcheck text time
    vector
  ];
  prePatch = "hpack";
  description = "Log Processing";
  license = lib.licenses.agpl3Only;
}
