{ mkDerivation, base, containers, lib, mtl, safe, template-haskell
, th-expand-syns
}:
mkDerivation {
  pname = "th-reify-many";
  version = "0.1.9";
  sha256 = "f889dd029d5ab191ace99fe595f363c60314d536e61c8c58f6167f1a9d29ae43";
  revision = "1";
  editedCabalFile = "0y0gyh866i40a71732mbkzb1clxh4cq26kma4xnrq86kdd7s2rr8";
  libraryHaskellDepends = [
    base containers mtl safe template-haskell th-expand-syns
  ];
  testHaskellDepends = [ base template-haskell ];
  homepage = "http://github.com/mgsloan/th-reify-many";
  description = "Recurseively reify template haskell datatype info";
  license = lib.licenses.bsd3;
}
