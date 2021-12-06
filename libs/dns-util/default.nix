{ mkDerivation, base, dns, hpack, hspec, hspec-discover, imports
, lib, polysemy, random
}:
mkDerivation {
  pname = "dns-util";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/dns-util;
  libraryHaskellDepends = [ base dns imports polysemy random ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base dns hspec imports polysemy random ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  description = "Library to deal with DNS SRV records";
  license = lib.licenses.agpl3Only;
}
