{ mkDerivation, base, ghc-prim, lib, primitive }:
mkDerivation {
  pname = "atomic-primops";
  version = "0.8.3";
  sha256 = "003af9f24f06355aa240d7ba11079def44d6e78f21e5869c5f0f677a7d6dc50e";
  libraryHaskellDepends = [ base ghc-prim primitive ];
  homepage = "https://github.com/rrnewton/haskell-lockfree/wiki";
  description = "A safe approach to CAS and other atomic ops in Haskell";
  license = lib.licenses.bsd3;
}
