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
  version = "0.3.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    sha256 = "sha256-slbBwvQgjHqkgd+FVzLhLBgH8L1fOBsS+P5wnPBfzTY=";
    rev = "244d96f7648bd2ac494dab7528c95d40230584a8";
  };
  doCheck = false;
  cargoSha256 = "sha256-UOB+fiHjz2xUP50CN766aT9TDVpd5Ebd+EDxrddmJbo=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-0.3.0-vendor.tar.gz/ring/.git
  '';
}
