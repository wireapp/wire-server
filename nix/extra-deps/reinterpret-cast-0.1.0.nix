{ mkDerivation, array, base, criterion, data-binary-ieee754, hspec
, lib, loop
}:
mkDerivation {
  pname = "reinterpret-cast";
  version = "0.1.0";
  sha256 = "5654622c904b42c62f2473c64624715dbd458ea00209ed9ab39396eabc1353e4";
  libraryHaskellDepends = [ array base ];
  testHaskellDepends = [ base data-binary-ieee754 hspec loop ];
  benchmarkHaskellDepends = [ base criterion data-binary-ieee754 ];
  homepage = "https://github.com/nh2/reinterpret-cast";
  description = "Memory reinterpretation casts for Float/Double and Word32/Word64";
  license = lib.licenses.mit;
}
