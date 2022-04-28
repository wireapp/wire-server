{ mkDerivation, aeson, ansi-terminal, base, bytestring
, case-insensitive, cookie, errors, exceptions, http-client
, http-types, imports, lens, lib, monad-control, mtl, text, tinylog
, transformers-base, types-common, unliftio, uri-bytestring, wai
, wai-extra
}:
mkDerivation {
  pname = "bilge";
  version = "0.22.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring case-insensitive cookie errors
    exceptions http-client http-types imports lens monad-control mtl
    text tinylog transformers-base types-common unliftio uri-bytestring
    wai wai-extra
  ];
  description = "Library for composing HTTP requests";
  license = lib.licenses.agpl3Only;
}
