{ mkDerivation, base, bytestring, containers, deepseq, extra, lib
, mtl, text, transformers, unliftio, unliftio-core
, unordered-containers
}:
mkDerivation {
  pname = "imports";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deepseq extra mtl text transformers
    unliftio unliftio-core unordered-containers
  ];
  description = "Very common imports";
  license = lib.licenses.agpl3Only;
}
