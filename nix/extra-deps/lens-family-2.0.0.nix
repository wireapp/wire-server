{ mkDerivation, base, containers, lens-family-core, lib, mtl
, transformers
}:
mkDerivation {
  pname = "lens-family";
  version = "2.0.0";
  sha256 = "6793f2a5c5030f02258532043d57eac42318cd7f9cef47f6720a7b99276f03db";
  revision = "1";
  editedCabalFile = "1nf0zxhwqkg54mc3kimnqcvg9b732rn35r1rjs1fzf0vwssla3zw";
  libraryHaskellDepends = [
    base containers lens-family-core mtl transformers
  ];
  description = "Lens Families";
  license = lib.licenses.bsd3;
}
