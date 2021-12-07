{ mkDerivation, base, bytestring, http-types, lib, mtl, QuickCheck
, tasty, tasty-quickcheck, unordered-containers, wai
}:
mkDerivation {
  pname = "wai-route";
  version = "0.4.0";
  sha256 = "d74799d74c106b3d5da30b51918fff67461ee44de42d018a377feb13f659b9e5";
  revision = "1";
  editedCabalFile = "0i243ymdihxyfcf0gng3y9a0d12g31g527p98xhsbr2554yz2lpf";
  libraryHaskellDepends = [
    base bytestring http-types unordered-containers wai
  ];
  testHaskellDepends = [
    base bytestring http-types mtl QuickCheck tasty tasty-quickcheck
    wai
  ];
  description = "Minimalistic, efficient routing for WAI";
  license = lib.licenses.mpl20;
}
