{ mkDerivation, base, ghc-boot-th, lib, pretty }:
mkDerivation {
  pname = "template-haskell";
  version = "2.15.0.0";
  sha256 = "948baba9a027f83acc6f4862ad78b97b04b3c66dd9b292cbf4eff855d6d7e89a";
  libraryHaskellDepends = [ base ghc-boot-th pretty ];
  description = "Support library for Template Haskell";
  license = lib.licenses.bsd3;
}
