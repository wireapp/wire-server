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
    rev = "77e8d5da81e1b3ac3d12c6f82d4b6311777119cb";
    sha256 = "sha256-E7qy/3AKBkuJOBgSiJwybBohP+3XC5zzQPUqU2RyFSc=";
  };
  doCheck = false;
  cargoPatches = [ ./crypto_cli.patch ];
  cargoSha256 = "sha256-4xtQoPuyeSiym2blWOHjbxBNO7clQKAMA3evWFqZxv8=";
}
