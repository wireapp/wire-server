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
    sha256 = "sha256-6G01eONZb/61MrO/Py+ix7Psz+jl+3Cn7xUMez3osxw=";
    rev = "d01258a290546a01a62dca21ba3d0e3863a288b4";
  };
  doCheck = false;
  cargoSha256 = "sha256-frzVXP0lxXhPhfNL4zleHj2WSMwmQfCdTqkTbHXBFEI=";
  cargoDepsHook = ''
    mkdir -p mls-test-cli-${version}-vendor.tar.gz/ring/.git
  '';
}
