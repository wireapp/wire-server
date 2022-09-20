{ mkDerivation, api-bot, api-client, base, bytestring
, bytestring-conversion, cereal, containers, exceptions, extended
, http-client, http-client-tls, imports, lens, lib, metrics-core
, mime, mwc-random, optparse-applicative, text, tinylog
, types-common, unliftio, uuid
}:
mkDerivation {
  pname = "api-simulations";
  version = "0.4.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    api-bot api-client base bytestring bytestring-conversion cereal
    containers exceptions imports lens text types-common
  ];
  executableHaskellDepends = [
    api-bot api-client base bytestring bytestring-conversion containers
    exceptions extended http-client http-client-tls imports lens
    metrics-core mime mwc-random optparse-applicative text tinylog
    types-common unliftio uuid
  ];
  description = "(Internal) Wire API simulations";
  license = lib.licenses.agpl3Only;
}
