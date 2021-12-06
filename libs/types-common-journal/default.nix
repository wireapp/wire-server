{ mkDerivation, base, bytestring, Cabal, imports, lib
, proto-lens-protoc, proto-lens-runtime, proto-lens-setup, time
, types-common, uuid
}:
mkDerivation {
  pname = "types-common-journal";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/types-common-journal;
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [
    base bytestring imports proto-lens-runtime time types-common uuid
  ];
  libraryToolDepends = [ proto-lens-protoc ];
  description = "Shared protobuf type definitions";
  license = lib.licenses.agpl3Only;
}
