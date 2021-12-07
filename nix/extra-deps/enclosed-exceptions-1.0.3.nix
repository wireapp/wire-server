{ mkDerivation, async, base, deepseq, hspec, lib, lifted-base
, monad-control, QuickCheck, stm, transformers, transformers-base
}:
mkDerivation {
  pname = "enclosed-exceptions";
  version = "1.0.3";
  sha256 = "af6d93f113ac92b89a32af1fed52f445f492afcc0be93980cbadc5698f94f0b9";
  libraryHaskellDepends = [
    base deepseq lifted-base monad-control transformers
    transformers-base
  ];
  testHaskellDepends = [
    async base deepseq hspec lifted-base monad-control QuickCheck stm
    transformers transformers-base
  ];
  homepage = "https://github.com/jcristovao/enclosed-exceptions";
  description = "Catching all exceptions from within an enclosed computation";
  license = lib.licenses.mit;
}
