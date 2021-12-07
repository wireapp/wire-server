{ mkDerivation, array, base, bytestring, containers, hashable
, indexed-profunctors, lib, mtl, optics-core, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "optics-extra";
  version = "0.2";
  sha256 = "a195bf9ae05486883a5995f6d026ebf1db08a5e3781c966fe069a1187d23c69a";
  libraryHaskellDepends = [
    array base bytestring containers hashable indexed-profunctors mtl
    optics-core text transformers unordered-containers vector
  ];
  description = "Extra utilities and instances for optics-core";
  license = lib.licenses.bsd3;
}
