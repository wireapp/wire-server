{ mkDerivation, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "text-short";
  version = "0.1.3";
  sha256 = "bf5b9fedb7d0301e8fdf33e3223d10ca940e9e72c18bac135be80b6016edd977";
  revision = "3";
  editedCabalFile = "1wjy98ihhipzr34b310sgjjq3cc12aydhckbrgr21kxkzwglm4nv";
  libraryHaskellDepends = [
    base binary bytestring deepseq ghc-prim hashable text
  ];
  testHaskellDepends = [
    base binary quickcheck-instances tasty tasty-hunit tasty-quickcheck
    text
  ];
  description = "Memory-efficient representation of Unicode text strings";
  license = lib.licenses.bsd3;
}
