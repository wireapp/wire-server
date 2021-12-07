{ mkDerivation, aeson, base, doctest, either, file-embed, hspec
, lib, mustache, optparse-applicative, pcre-light, QuickCheck, rio
, template-haskell, text, time, yaml
}:
mkDerivation {
  pname = "headroom";
  version = "0.2.1.0";
  sha256 = "dda3f31c1add96c716aaa56d77a5c7fa5926b7a63fc5ca605f2a7bc18289912a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base either file-embed mustache optparse-applicative
    pcre-light rio template-haskell text time yaml
  ];
  executableHaskellDepends = [ base optparse-applicative rio ];
  testHaskellDepends = [
    aeson base doctest hspec optparse-applicative pcre-light QuickCheck
    rio
  ];
  homepage = "https://github.com/vaclavsvejcar/headroom";
  description = "License Header Manager";
  license = lib.licenses.bsd3;
}
