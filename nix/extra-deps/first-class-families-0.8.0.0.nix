{ mkDerivation, base, doctest, Glob, lib }:
mkDerivation {
  pname = "first-class-families";
  version = "0.8.0.0";
  sha256 = "5b756d9a5941fc4b388843a3805aff3e0049bf4f1da4ab505d93bea3f7a012a4";
  revision = "1";
  editedCabalFile = "02z6wixk9kdgshxsz99lag29lb70kadg9wn6vsgk906wj014fv52";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest Glob ];
  homepage = "https://github.com/Lysxia/first-class-families#readme";
  description = "First class type families";
  license = lib.licenses.mit;
}
