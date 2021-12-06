{ mkDerivation, base, byteable, bytestring, hpack, HsOpenSSL
, http-client, imports, lib, time
}:
mkDerivation {
  pname = "ssl-util";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/ssl-util;
  libraryHaskellDepends = [
    base byteable bytestring HsOpenSSL http-client imports time
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "SSL-related utilities";
  license = lib.licenses.agpl3Only;
}
