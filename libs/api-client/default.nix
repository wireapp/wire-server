{ mkDerivation, aeson, async, base, bilge, bytestring
, bytestring-conversion, connection, cookie, data-default-class
, errors, exceptions, hpack, http-client, http-types, imports, lib
, mime, retry, text, time, tinylog, transformers, types-common
, unliftio, unordered-containers, uuid, websockets, wire-api
}:
mkDerivation {
  pname = "api-client";
  version = "0.4.2";
  src = /home/axeman/workspace/wire-server/libs/api-client;
  libraryHaskellDepends = [
    aeson async base bilge bytestring bytestring-conversion connection
    cookie data-default-class errors exceptions http-client http-types
    imports mime retry text time tinylog transformers types-common
    unliftio unordered-containers uuid websockets wire-api
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "(Internal) Wire HTTP API Client";
  license = lib.licenses.agpl3Only;
}
