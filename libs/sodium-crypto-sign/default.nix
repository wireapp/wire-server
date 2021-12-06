{ mkDerivation, base, base64-bytestring, bytestring, hpack, imports
, lib, libsodium
}:
mkDerivation {
  pname = "sodium-crypto-sign";
  version = "0.1.2";
  src = /home/axeman/workspace/wire-server/libs/sodium-crypto-sign;
  libraryHaskellDepends = [
    base base64-bytestring bytestring imports
  ];
  libraryPkgconfigDepends = [ libsodium ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "FFI to some of the libsodium crypto_sign_* functions";
  license = lib.licenses.agpl3Only;
}
