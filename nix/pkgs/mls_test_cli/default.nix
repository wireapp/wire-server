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
  version = "0.4.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    sha256 = "sha256-Gw1+b7kslc/KcB+pEqP1FuE6tAPqKtB6hlkLcXMuCcM=";
    rev = "f44dec2705e1833b654cb6f02271e11a6c2fdeb0";
  };
  doCheck = false;
  cargoSha256 = "sha256-3zUGEowQREPKsfpH2y9C7BeeTTF3zat4Qfpw74fOCHQ=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
