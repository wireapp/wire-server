{ mkDerivation, base, Cabal, lib, proto-lens-protoc
, proto-lens-runtime, proto-lens-setup
}:
mkDerivation {
  pname = "wire-message-proto-lens";
  version = "0.1.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [ base proto-lens-runtime ];
  libraryToolDepends = [ proto-lens-protoc ];
  description = "Shared protobuf type definitions for Wire Messaging";
  license = lib.licenses.agpl3Only;
}
