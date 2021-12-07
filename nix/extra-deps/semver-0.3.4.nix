{ mkDerivation, attoparsec, base, criterion, deepseq, hashable, lib
, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "semver";
  version = "0.3.4";
  sha256 = "42dbdacb08f30ac8bf2f014981cb080737f793b89d57626cb7e2ab8c3d768e6b";
  libraryHaskellDepends = [ attoparsec base deepseq hashable text ];
  testHaskellDepends = [ base tasty tasty-hunit text ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://github.com/brendanhay/semver";
  description = "Representation, manipulation, and de/serialisation of Semantic Versions";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
