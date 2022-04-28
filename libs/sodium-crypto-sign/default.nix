{ mkDerivation, base, base64-bytestring, bytestring, imports, lib
, libsodium
}:
mkDerivation {
  pname = "sodium-crypto-sign";
  version = "0.1.2";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring imports
  ];
  libraryPkgconfigDepends = [ libsodium ];
  description = "FFI to some of the libsodium crypto_sign_* functions";
  license = lib.licenses.agpl3Only;
}
