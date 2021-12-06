{ mkDerivation, base, bytestring, containers, deepseq, extra, hpack
, lib, mtl, text, transformers, unliftio, unliftio-core
, unordered-containers
}:
mkDerivation {
  pname = "imports";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/imports;
  libraryHaskellDepends = [
    base bytestring containers deepseq extra mtl text transformers
    unliftio unliftio-core unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Very common imports";
  license = lib.licenses.agpl3Only;
}
