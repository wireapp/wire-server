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
    sha256 = "sha256-FjgAcYdUr/ZWdQxbck2UEG6NEEQLuz0S4a55hrAxUs4=";
    rev = "82fc148964ef5baa92a90d086fdc61adaa2b5dbf";
  };
  doCheck = false;
  cargoSha256 = "sha256-AlZrxa7f5JwxxrzFBgeFSaYU6QttsUpfLYfq1HzsdbE=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
