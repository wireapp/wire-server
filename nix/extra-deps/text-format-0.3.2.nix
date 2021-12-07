{ mkDerivation, array, base, double-conversion, ghc-prim
, integer-gmp, lib, old-locale, text, time, transformers
}:
mkDerivation {
  pname = "text-format";
  version = "0.3.2";
  sha256 = "326637b8ad8420a51c0531cb444e45e0029d68c5980a53d5ffdfa2297d47bae3";
  revision = "2";
  editedCabalFile = "05findgw709h930wshaq514maxarjyjhsam6pkyzq83iz1yc2gra";
  libraryHaskellDepends = [
    array base double-conversion ghc-prim integer-gmp old-locale text
    time transformers
  ];
  homepage = "https://github.com/bos/text-format";
  description = "Text formatting";
  license = lib.licenses.bsd3;
}
