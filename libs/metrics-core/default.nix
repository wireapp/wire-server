{ mkDerivation, base, containers, hashable, hpack, immortal
, imports, lib, prometheus-client, text, unordered-containers
}:
mkDerivation {
  pname = "metrics-core";
  version = "0.3.2";
  src = /home/axeman/workspace/wire-server/libs/metrics-core;
  libraryHaskellDepends = [
    base containers hashable immortal imports prometheus-client text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Metrics core";
  license = lib.licenses.agpl3Only;
}
