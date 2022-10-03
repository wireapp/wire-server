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
    sha256 = "sha256-xYL9KNcirCARb1Rp41einOpq0ut5adlqMIAEiwYXkzg=";
    rev = "d46624fb49c900facc8853fa86e3ecf51fd0dcdb";
  };
  doCheck = false;
  cargoSha256 = "sha256-FGFyS/tLlD+3JQX7vkKq4nW+WQI1FFnpugzfFBi/eQE=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
