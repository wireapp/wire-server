{ mkDerivation, aeson, base, bytestring, containers, criterion
, deepseq, directory, file-embed, filepath, hspec, hspec-discover
, hspec-megaparsec, lib, megaparsec, mtl, template-haskell, text
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "stache";
  version = "2.1.1";
  sha256 = "8d125b5f4f5b967b260137966278d9968432234ba20ea8177169be5dea3df61a";
  revision = "2";
  editedCabalFile = "1a25mwi1x3yqq9clm9gz0dibpnppznbx392ixfwc21hnngn7kxsp";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq directory filepath
    megaparsec mtl template-haskell text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers file-embed hspec hspec-megaparsec
    megaparsec template-haskell text yaml
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    aeson base criterion deepseq megaparsec text
  ];
  homepage = "https://github.com/stackbuilders/stache";
  description = "Mustache templates for Haskell";
  license = lib.licenses.bsd3;
}
