{ mkDerivation, aeson, base, bilge, brig-types, bytestring
, bytestring-conversion, containers, data-default, errors
, exceptions, extended, galley-types, gundeck-types, http-client
, http-types, imports, lens, lib, metrics-wai, mtl
, schema-profunctor, split, string-conversions, swagger, text
, tinylog, transformers, types-common, unliftio
, unordered-containers, uuid, wai, wai-extra, wai-predicates
, wai-routing, wai-utilities, warp, wire-api, yaml
}:
mkDerivation {
  pname = "stern";
  version = "1.7.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bilge brig-types bytestring bytestring-conversion
    containers data-default errors exceptions extended galley-types
    gundeck-types http-client http-types imports lens metrics-wai mtl
    schema-profunctor split string-conversions swagger text tinylog
    transformers types-common unliftio unordered-containers uuid wai
    wai-extra wai-predicates wai-routing wai-utilities warp wire-api
    yaml
  ];
  executableHaskellDepends = [
    base extended imports types-common unliftio
  ];
  license = lib.licenses.agpl3Only;
  mainProgram = "stern";
}
