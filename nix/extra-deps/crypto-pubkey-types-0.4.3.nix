{ mkDerivation, asn1-encoding, asn1-types, base, lib }:
mkDerivation {
  pname = "crypto-pubkey-types";
  version = "0.4.3";
  sha256 = "7ed9f52281ec4e34021a91818fe45288e33d65bff937f60334a3f45be5a71c60";
  libraryHaskellDepends = [ asn1-encoding asn1-types base ];
  homepage = "http://github.com/vincenthz/hs-crypto-pubkey-types";
  description = "Generic cryptography Public keys algorithm types";
  license = lib.licenses.bsd3;
}
