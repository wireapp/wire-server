{ mkDerivation, base, containers, deepseq, lib, tasty, tasty-hunit
, text
}:
mkDerivation {
  pname = "wl-pprint-annotated";
  version = "0.1.0.1";
  sha256 = "6b662b244b2e318a2923dc7057d707369a29ea4a0e721b4710eac7239cc727af";
  libraryHaskellDepends = [ base containers deepseq text ];
  testHaskellDepends = [
    base containers deepseq tasty tasty-hunit text
  ];
  homepage = "https://github.com/minad/wl-pprint-annotated#readme";
  description = "Pretty printer with annotation support";
  license = lib.licenses.bsd3;
}
