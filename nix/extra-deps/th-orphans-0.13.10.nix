{ mkDerivation, base, bytestring, ghc-prim, hspec, hspec-discover
, lib, mtl, template-haskell, th-lift, th-lift-instances
, th-reify-many
}:
mkDerivation {
  pname = "th-orphans";
  version = "0.13.10";
  sha256 = "4433573649ccf97e475c58f65eb24af0100a954ce2c801c34b8f53157796c928";
  libraryHaskellDepends = [
    base mtl template-haskell th-lift th-lift-instances th-reify-many
  ];
  testHaskellDepends = [
    base bytestring ghc-prim hspec template-haskell th-lift
  ];
  testToolDepends = [ hspec-discover ];
  description = "Orphan instances for TH datatypes";
  license = lib.licenses.bsd3;
}
