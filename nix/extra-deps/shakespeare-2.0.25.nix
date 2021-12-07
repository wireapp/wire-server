{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, containers, directory, exceptions, ghc-prim, hspec, HUnit, lib
, parsec, process, scientific, template-haskell, text, th-lift
, time, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "shakespeare";
  version = "2.0.25";
  sha256 = "4427b923ee466525352ab5209eae2faabc929c1b14c0d8463ba815419e1f5bba";
  revision = "1";
  editedCabalFile = "0na31a7h3sq8ndrx79waywsfj5667pm0masy10gxzhzwmf6i3s1l";
  libraryHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers directory
    exceptions ghc-prim parsec process scientific template-haskell text
    th-lift time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers directory
    exceptions ghc-prim hspec HUnit parsec process template-haskell
    text time transformers
  ];
  homepage = "http://www.yesodweb.com/book/shakespearean-templates";
  description = "A toolkit for making compile-time interpolated templates";
  license = lib.licenses.mit;
}
