{ mkDerivation, array, base, bytestring, containers, lib
, semigroups, unordered-containers
}:
mkDerivation {
  pname = "charset";
  version = "0.3.7.1";
  sha256 = "3d415d2883bd7bf0cc9f038e8323f19c71e07dd12a3c712f449ccb8b4daac0be";
  revision = "2";
  editedCabalFile = "002x3yan7632nqgwk0a7f3wvchgm95pdwqh225va8dnn1lr9pi1z";
  libraryHaskellDepends = [
    array base bytestring containers semigroups unordered-containers
  ];
  homepage = "http://github.com/ekmett/charset";
  description = "Fast unicode character sets based on complemented PATRICIA tries";
  license = lib.licenses.bsd3;
}
