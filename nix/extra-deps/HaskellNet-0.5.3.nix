{ mkDerivation, array, base, base64-string, bytestring
, cryptohash-md5, lib, mime-mail, mtl, network, network-bsd
, old-time, pretty, text
}:
mkDerivation {
  pname = "HaskellNet";
  version = "0.5.3";
  sha256 = "dad23cec0ac1c5be257281bc97cb1832ce6ca9e45673c4a28c782de370537974";
  libraryHaskellDepends = [
    array base base64-string bytestring cryptohash-md5 mime-mail mtl
    network network-bsd old-time pretty text
  ];
  homepage = "https://github.com/qnikst/HaskellNet";
  description = "Client support for POP3, SMTP, and IMAP";
  license = lib.licenses.bsd3;
}
