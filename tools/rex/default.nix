{ mkDerivation, async, attoparsec, base, bytestring, clock, dns
, exceptions, http-types, iproute, lib, mtl, network
, optparse-applicative, prometheus, text, tinylog
, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "rex";
  version = "0.3.0";
  src = /home/axeman/workspace/wire-server/tools/rex;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async attoparsec base bytestring clock dns exceptions http-types
    iproute mtl network optparse-applicative prometheus text tinylog
    unordered-containers wai warp
  ];
  description = "Scrape and expose restund metrics for prometheus";
  license = lib.licenses.agpl3Only;
}
