{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "54ddf08e7ff429446426842c3debccd22a744e7e";
    sha256 = "sha256-3SjbhCxEovIoVTmEOl7Ti84wYY1Re7ZdeDhOP4BTVHM=";
  };
  pname = "mls-test-cli";
  version = "0.11";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-1.0.0" = "sha256-a3w/ZoIedcSmJLYvpo7pkCzxvPE9nwGx3owyj87h/Uo=";
    };
  };
  doCheck = false;
}
