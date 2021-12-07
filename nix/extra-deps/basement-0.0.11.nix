{ mkDerivation, base, ghc-prim, lib }:
mkDerivation {
  pname = "basement";
  version = "0.0.11";
  sha256 = "67582b3475a5547925399f719df21f8bbbd0ca4d4db27795c22a474f8ee6346b";
  revision = "3";
  editedCabalFile = "1indgsrk0yhkbqlxj39qqb5xqicwkmcliggb8wn87vgfswxpi1dn";
  libraryHaskellDepends = [ base ghc-prim ];
  homepage = "https://github.com/haskell-foundation/foundation#readme";
  description = "Foundation scrap box of array & string";
  license = lib.licenses.bsd3;
}
