{ mkDerivation, array, base, base16-bytestring, base64-bytestring
, bytestring, connection, cryptohash, filepath, lib, mime-mail
, network, network-bsd, text
}:
mkDerivation {
  pname = "smtp-mail";
  version = "0.2.0.0";
  sha256 = "5fb146fb71849c076cdd8a3d027a5155afa9af96eb0c60958cd9e601e0745f61";
  revision = "1";
  editedCabalFile = "1kv84kywyj8f7iypzdq6a32wwkk8318khhy4x3p9q6mlvgv8275r";
  libraryHaskellDepends = [
    array base base16-bytestring base64-bytestring bytestring
    connection cryptohash filepath mime-mail network network-bsd text
  ];
  homepage = "http://github.com/jhickner/smtp-mail";
  description = "Simple email sending via SMTP";
  license = lib.licenses.bsd3;
}
