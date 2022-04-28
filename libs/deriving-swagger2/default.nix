{ mkDerivation, base, extra, imports, lib, swagger2 }:
mkDerivation {
  pname = "deriving-swagger2";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base extra imports swagger2 ];
  license = lib.licenses.agpl3Only;
}
