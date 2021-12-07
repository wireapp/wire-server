{ mkDerivation, array, base, lib }:
mkDerivation {
  pname = "strict";
  version = "0.3.2";
  sha256 = "2cd35a67938db635a87617d9576d5df0158b581e8e5694f07487c0f4b1549221";
  libraryHaskellDepends = [ array base ];
  homepage = "http://www.cse.unsw.edu.au/~rl/code/strict.html";
  description = "Strict data types and String IO";
  license = lib.licenses.bsd3;
}
