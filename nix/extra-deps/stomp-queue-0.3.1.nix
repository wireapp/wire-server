{ mkDerivation, attoparsec, base, bytestring, conduit
, conduit-extra, lib, mime, mtl, network-conduit-tls, split, stompl
, time, utf8-string
}:
mkDerivation {
  pname = "stomp-queue";
  version = "0.3.1";
  sha256 = "47dd7f332f2544aaee66fa37ff889cd666c98a8401cec1deeede01b0730b20bb";
  libraryHaskellDepends = [
    attoparsec base bytestring conduit conduit-extra mime mtl
    network-conduit-tls split stompl time utf8-string
  ];
  homepage = "http://github.com/toschoo/mom";
  description = "Stompl Client Library";
  license = "LGPL";
}
