{ mkDerivation, base, byteable, bytestring, HsOpenSSL, http-client
, imports, lib, time
}:
mkDerivation {
  pname = "ssl-util";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base byteable bytestring HsOpenSSL http-client imports time
  ];
  description = "SSL-related utilities";
  license = lib.licenses.agpl3Only;
}
