{ mkDerivation, base, hashable, HUnit, lib, stm, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "async";
  version = "2.2.2";
  sha256 = "4b4ab1ac82c45144d82c6daf6cb6ba45eab9957dad44787fa5e869e23d73bbff";
  revision = "1";
  editedCabalFile = "1kg9xmby0wkx31998h2r43yr8bl1aixk6025zqigz9vdhmkc2y51";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hashable stm ];
  testHaskellDepends = [
    base HUnit stm test-framework test-framework-hunit
  ];
  homepage = "https://github.com/simonmar/async";
  description = "Run IO operations asynchronously and wait for their results";
  license = lib.licenses.bsd3;
}
