{ mkDerivation, base, containers, lib, mtl, optics-core
, template-haskell, th-abstraction, transformers
}:
mkDerivation {
  pname = "optics-th";
  version = "0.2";
  sha256 = "683b64191056a730dcb2927be1f8c937b985754b60af7f607b01e3575692118a";
  libraryHaskellDepends = [
    base containers mtl optics-core template-haskell th-abstraction
    transformers
  ];
  testHaskellDepends = [ base optics-core ];
  description = "Optics construction using TemplateHaskell";
  license = lib.licenses.bsd3;
}
