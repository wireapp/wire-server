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
    rev = "4befbebfd5575537167c5952db6e9967f2076001";
    sha256 = "sha256-j1n68VVs91uzQ9vwSuIHIMXxVZV/Xeto+V+69ErmL0Q=";
  };
  doCheck = false;
  cargoSha256 = "sha256-6257yC+NyJu4PNLVQUrp+YVOOpvcuUoT9Hd9+uq+73w=";
}
