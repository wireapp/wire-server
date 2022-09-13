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
    sha256 = "sha256-nBtXkxGstSqBEhzjcRd0RG2hv0WFgTqy1z29W2sf27U=";
    rev = "560186482d201fe0f6194d620dba2b623fdd7f6f";
  };
  doCheck = false;
  cargoSha256 = "sha256-3zUGEowQREPKsfpH2y9C7BeeTTF3zat4Qfpw74fOCHQ=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
