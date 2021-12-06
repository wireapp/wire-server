{ mkDerivation, aeson, base, bilge, brig-types, bytestring
, bytestring-conversion, data-default, errors, exceptions, extended
, galley-types, gundeck-types, hpack, http-client, http-types
, imports, lens, lib, metrics-wai, mtl, split, string-conversions
, swagger, text, tinylog, transformers, types-common, unliftio
, unordered-containers, uuid, wai, wai-extra, wai-predicates
, wai-routing, wai-utilities, warp, wire-api, yaml
}:
mkDerivation {
  pname = "stern";
  version = "1.7.2";
  src = /home/axeman/workspace/wire-server/tools/stern;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bilge brig-types bytestring bytestring-conversion
    data-default errors exceptions extended galley-types gundeck-types
    http-client http-types imports lens metrics-wai mtl split
    string-conversions swagger text tinylog transformers types-common
    unliftio unordered-containers uuid wai wai-extra wai-predicates
    wai-routing wai-utilities warp wire-api yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base extended imports types-common unliftio
  ];
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
