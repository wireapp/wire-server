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
    rev = "d1d2798a9c40ed1a8c622055d977fb0651bd9658";
    sha256 = "sha256-QtDWoKCX6Us1w/KFb7nAcc1YfmiIoQsJI7ftvPNqYkU=";
  };
  doCheck = false;
  cargoPatches = [ ./crypto_cli.patch ];
  cargoSha256 = "sha256-ql6j3e6J0pI6gXiaGhjDkL09i1hg/PFEj6hhBIqzKbo=";
}
