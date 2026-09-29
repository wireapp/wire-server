{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "ec42d3d386efdb711ce6ba8f0d624d09d71f018e";
    sha256 = "sha256-kATLideHHkscpeC+LE/pttfM9ixrprV8ZBuC+qw3qiM=";
  };
  pname = "mls-test-cli";
  version = "0.12.0";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-1.0.0" = "sha256-a3w/ZoIedcSmJLYvpo7pkCzxvPE9nwGx3owyj87h/Uo=";
    };
  };
  doCheck = false;
}
