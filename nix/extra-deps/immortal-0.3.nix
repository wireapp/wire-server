{ mkDerivation, base, lib, stm, tasty, tasty-hunit, transformers
, unliftio-core
}:
mkDerivation {
  pname = "immortal";
  version = "0.3";
  sha256 = "11c89db97f33c8bbfe6f72c728c68135a247608ceb2335dfb7ac6679acb41f88";
  libraryHaskellDepends = [ base stm unliftio-core ];
  testHaskellDepends = [ base stm tasty tasty-hunit transformers ];
  homepage = "https://github.com/feuerbach/immortal";
  description = "Spawn threads that never die (unless told to do so)";
  license = lib.licenses.mit;
}
