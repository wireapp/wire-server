{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, crypto-pubkey-types, data-default, http-client, http-types, lib
, random, RSA, SHA, time, transformers, transformers-compat
}:
mkDerivation {
  pname = "authenticate-oauth";
  version = "1.6.0.1";
  sha256 = "e0520fb4255ac8d6ff30f06a2b91a9fdc478aa799e254e52747ebd13d70f3ec3";
  revision = "2";
  editedCabalFile = "08i6mmk2jqlrd1aksjx02arly7dfpkwc0dwxpr7hs4rbxajbckyr";
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder bytestring crypto-pubkey-types
    data-default http-client http-types random RSA SHA time
    transformers transformers-compat
  ];
  homepage = "http://github.com/yesodweb/authenticate";
  description = "Library to authenticate with OAuth for Haskell web applications";
  license = lib.licenses.bsd3;
}
