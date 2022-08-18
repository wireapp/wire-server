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
    sha256 = "sha256-5CjGd7Di58XvseC0zmkU2Xc5t2qH/g1a6cjDDQvrCsU=";
    rev = "aeead948a40d968119c847741f4610a25ab94595";
  };
  doCheck = false;
  cargoSha256 = "sha256-UOB+fiHjz2xUP50CN766aT9TDVpd5Ebd+EDxrddmJbo=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-0.3.0-vendor.tar.gz/ring/.git
  '';
}
