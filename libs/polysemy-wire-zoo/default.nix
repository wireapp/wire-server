{ mkDerivation, base, hpack, imports, lib, polysemy, tinylog }:
mkDerivation {
  pname = "polysemy-wire-zoo";
  version = "0.1.0";
  src = /home/axeman/workspace/wire-server/libs/polysemy-wire-zoo;
  libraryHaskellDepends = [ base imports polysemy tinylog ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Polysemy interface for various libraries";
  license = lib.licenses.agpl3Only;
}
