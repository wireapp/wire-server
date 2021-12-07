{ mkDerivation, base, lib }:
mkDerivation {
  pname = "base-prelude";
  version = "1.3";
  sha256 = "e3cc66e99d6c83aac548c4d8e6a166e5bd9cf557947cde49161026d0341267fe";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/nikita-volkov/base-prelude";
  description = "The most complete prelude formed solely from the \"base\" package";
  license = lib.licenses.mit;
}
