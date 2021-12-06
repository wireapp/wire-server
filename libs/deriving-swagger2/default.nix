{ mkDerivation, base, extra, hpack, imports, lib, swagger2 }:
mkDerivation {
  pname = "deriving-swagger2";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/deriving-swagger2;
  libraryHaskellDepends = [ base extra imports swagger2 ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
