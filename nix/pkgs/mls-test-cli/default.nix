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
  version = "0.6.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    sha256 = "sha256-/XQ/9oQTPkRqgMzDGRm+Oh9jgkdeDM1vRJ6/wEf2+bY=";
    rev = "c6f80be2839ac1ed2894e96044541d1c3cf6ecdf";
  };
  doCheck = false;
  cargoSha256 = "sha256-AlZrxa7f5JwxxrzFBgeFSaYU6QttsUpfLYfq1HzsdbE=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
