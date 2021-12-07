{ mkDerivation, base, bytestring, containers, filepath, ghc
, ghc-paths, ghc-source-gen, lens-family, lib, pretty, proto-lens
, proto-lens-runtime, protobuf, text
}:
mkDerivation {
  pname = "proto-lens-protoc";
  version = "0.7.0.0";
  sha256 = "7afb845390f7311cf43b991b73adab6210f1760a53b4c38cfc959456320506cc";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base filepath ];
  libraryToolDepends = [ protobuf ];
  executableHaskellDepends = [
    base bytestring containers filepath ghc ghc-paths ghc-source-gen
    lens-family pretty proto-lens proto-lens-runtime text
  ];
  homepage = "https://github.com/google/proto-lens#readme";
  description = "Protocol buffer compiler for the proto-lens library";
  license = lib.licenses.bsd3;
}
