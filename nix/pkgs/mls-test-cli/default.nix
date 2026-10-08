{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "e93e93a015a9e9e8d4a4a1f436d79960df598c22";
    sha256 = "sha256-zWtogZrniuPV6b23+LRGpWu8Dc9dj1T9JUe3dqX26ko=";
  };
  pname = "mls-test-cli";
  version = "0.13.1";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-1.0.0" = "sha256-a3w/ZoIedcSmJLYvpo7pkCzxvPE9nwGx3owyj87h/Uo=";
    };
  };
  doCheck = false;
}
