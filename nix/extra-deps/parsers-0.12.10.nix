{ mkDerivation, attoparsec, base, base-orphans, binary, bytestring
, charset, containers, lib, mtl, parsec, QuickCheck
, quickcheck-instances, scientific, semigroups, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "parsers";
  version = "0.12.10";
  sha256 = "17b91f1318ca54679395b382a056df633fdb44fbb962eca66b1787f957af1a6c";
  revision = "1";
  editedCabalFile = "15pcq6iz3mbcwlknzxxfgd1myhyl41fs4j3m80pkif6dm5g18rv3";
  libraryHaskellDepends = [
    attoparsec base base-orphans binary charset containers mtl parsec
    scientific semigroups text transformers unordered-containers
  ];
  testHaskellDepends = [
    attoparsec base bytestring parsec QuickCheck quickcheck-instances
  ];
  homepage = "http://github.com/ekmett/parsers/";
  description = "Parsing combinators";
  license = lib.licenses.bsd3;
}
