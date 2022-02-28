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
    rev = "bb9e7b6d21beaebcb4278dee1a9e6feaa77e711f";
    sha256 = "sha256-3D524DTgiNTqTlBIzCzTktCyYS0iA2TSi7axvzrvPLU=";
  };
  cargoSha256 = "sha256-he7cytqEhVFiPw4bVAjveh2xQE0nO1dE4yzAVWz6sSc=";
}

