{ mkDerivation, base, lib }:
mkDerivation {
  pname = "transformers";
  version = "0.5.6.2";
  sha256 = "b668795d600297e4c8a7fd55a107b9827b2c52c0bc14c5ea0d65e20e6691c66c";
  libraryHaskellDepends = [ base ];
  description = "Concrete functor and monad transformers";
  license = lib.licenses.bsd3;
}
