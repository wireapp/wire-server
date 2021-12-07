{ mkDerivation, async, base, base-compat-batteries, bytestring
, clock, containers, criterion, deepseq, HUnit, lib, process
, random, tf-random, time, vector
}:
mkDerivation {
  pname = "splitmix";
  version = "0.0.4";
  sha256 = "aaa1b31a5320fd6a7d69a7f485ba8facdccf2253a1431feddaacc08ab2943091";
  libraryHaskellDepends = [ base deepseq random time ];
  testHaskellDepends = [
    async base base-compat-batteries bytestring deepseq HUnit process
    random tf-random vector
  ];
  benchmarkHaskellDepends = [
    base clock containers criterion random tf-random
  ];
  description = "Fast Splittable PRNG";
  license = lib.licenses.bsd3;
}
