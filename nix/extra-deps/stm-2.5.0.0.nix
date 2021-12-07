{ mkDerivation, array, base, lib }:
mkDerivation {
  pname = "stm";
  version = "2.5.0.0";
  sha256 = "59e3685c66cbc54770d423f097ce50661005c99160be0f43a2b7fef7916494c6";
  revision = "1";
  editedCabalFile = "189fxk75h7n27kw7ndyn8nkxm3117qdh1dpag1mcs487kxghff62";
  libraryHaskellDepends = [ array base ];
  homepage = "https://wiki.haskell.org/Software_transactional_memory";
  description = "Software Transactional Memory";
  license = lib.licenses.bsd3;
}
