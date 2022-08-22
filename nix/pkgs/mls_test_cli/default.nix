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
    sha256 = "sha256:1x8i39ivhf458jsz690rx2zsn01rkq3kvf7l5w1lfxxgypszd1lv";
    rev = "0f3ae8c5256570b197f0f4e3a51b5e6f371bb38f";
  };
  doCheck = false;
  cargoSha256 = "sha256:1fi5cvbsvwa0z3fldr2xb86m6gv9paz3f0lx7xa6rkz345z7xq2h";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
