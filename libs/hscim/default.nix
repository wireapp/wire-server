# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, aeson-qq
, attoparsec
, base
, bytestring
, case-insensitive
, email-validate
, gitignoreSource
, hashable
, hedgehog
, hspec
, hspec-discover
, hspec-expectations
, hspec-wai
, http-api-data
, http-media
, http-types
, hw-hspec-hedgehog
, indexed-traversable
, lib
, list-t
, microlens
, mmorph
, mtl
, network-uri
, retry
, scientific
, servant
, servant-client
, servant-client-core
, servant-server
, stm
, stm-containers
, string-conversions
, template-haskell
, text
, time
, uuid
, wai
, wai-extra
, warp
}:
mkDerivation {
  pname = "hscim";
  version = "0.4.0.2";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    aeson-qq
    attoparsec
    base
    bytestring
    case-insensitive
    email-validate
    hashable
    hspec
    hspec-expectations
    hspec-wai
    http-api-data
    http-media
    http-types
    list-t
    microlens
    mmorph
    mtl
    network-uri
    retry
    scientific
    servant
    servant-client
    servant-client-core
    servant-server
    stm
    stm-containers
    string-conversions
    template-haskell
    text
    time
    uuid
    wai
    wai-extra
  ];
  executableHaskellDepends = [
    base
    email-validate
    network-uri
    stm
    stm-containers
    time
    warp
  ];
  testHaskellDepends = [
    aeson
    attoparsec
    base
    bytestring
    email-validate
    hedgehog
    hspec
    hspec-expectations
    hspec-wai
    http-types
    hw-hspec-hedgehog
    indexed-traversable
    microlens
    network-uri
    servant
    servant-server
    stm-containers
    text
    wai
    wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/wireapp/wire-server/libs/hscim/README.md";
  description = "hscim json schema and server implementation";
  license = lib.licenses.agpl3Only;
  mainProgram = "hscim-server";
}
