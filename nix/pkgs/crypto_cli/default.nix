{ fetchFromGitHub
, lib
, libsodium
, perl
, pkg-config
, rustPlatform
, stdenv
}:

rustPlatform.buildRustPackage rec {
  name = "crypto-cli-${version}";
  version = "0.1.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "core-crypto";
    rev = "17eefbb043b6155a6a0b49a067ac29cfe6febfb2";
    sha256 = "sha256-9Z4PqYGoz/zL9lQSj67WXQgHHuVsATn3prKPMQqXpt4=";
  };
  doCheck = false;
  cargoPatches = [ ./crypto_cli.patch ];
  cargoSha256 = "sha256-ql6j3e6J0pI6gXiaGhjDkL09i1hg/PFEj6hhBIqzKbo=";
}

