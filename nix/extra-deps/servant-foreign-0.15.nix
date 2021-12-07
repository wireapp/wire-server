{ mkDerivation, base, base-compat, hspec, hspec-discover
, http-types, lens, lib, servant, text
}:
mkDerivation {
  pname = "servant-foreign";
  version = "0.15";
  sha256 = "f1197f1319a735b37c5fdd991556bf34b780a9b87d0e57d936a42ae6734bbd73";
  revision = "2";
  editedCabalFile = "0axz78g0vhasq5cvqg1lq0b2qanmb768f1bvzbfx58rn6arwflnj";
  libraryHaskellDepends = [
    base base-compat http-types lens servant text
  ];
  testHaskellDepends = [ base hspec servant ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "Helpers for generating clients for servant APIs in any programming language";
  license = lib.licenses.bsd3;
}
