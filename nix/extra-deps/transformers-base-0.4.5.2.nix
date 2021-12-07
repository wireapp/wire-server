{ mkDerivation, base, base-orphans, lib, stm, transformers
, transformers-compat
}:
mkDerivation {
  pname = "transformers-base";
  version = "0.4.5.2";
  sha256 = "d0c80c63fdce6a077dd8eda4f1ff289b85578703a3f1272e141d400fe23245e8";
  revision = "1";
  editedCabalFile = "18j122ypx12rbl9bbf622fvj8nif4rsci8z4qw2llmznbvfl09s0";
  libraryHaskellDepends = [
    base base-orphans stm transformers transformers-compat
  ];
  homepage = "https://github.com/mvv/transformers-base";
  description = "Lift computations from the bottom of a transformer stack";
  license = lib.licenses.bsd3;
}
