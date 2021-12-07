{ mkDerivation, array, base, containers, lib, random, time }:
mkDerivation {
  pname = "abstract-deque";
  version = "0.3";
  sha256 = "09aa10f38193a8275a7791b92a4f3a7192a304874637e2a35c897dde25d75ca2";
  libraryHaskellDepends = [ array base containers random time ];
  homepage = "https://github.com/rrnewton/haskell-lockfree/wiki";
  description = "Abstract, parameterized interface to mutable Deques";
  license = lib.licenses.bsd3;
}
