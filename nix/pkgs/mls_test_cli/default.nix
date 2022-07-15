{ fetchFromGitHub
, lib
, libsodium
, perl
, pkg-config
, rustPlatform
, stdenv
}:

rustPlatform.buildRustPackage rec {
  name = "mls-test-cli-${version}";
  version = "0.2.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "05cc435cfc16c0fb68434546ca4578ca35ecf550";
    sha256 = "sha256-Gd9LwWULGKolyaYJpcdK4KpneBf6jEaZqE7LjsRkY9E=";
  };
  doCheck = false;
  cargoSha256 = "sha256-IdzcCrYJgaoxKTuJ0e1GPe0a5P1egBWmSKt9/or9nrM=";
}
