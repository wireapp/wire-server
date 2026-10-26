{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "3df2defdfb2b4a66382d62cf4aa50f2c6c3c1700";
    sha256 = "sha256-rmj2RjyXIEFq12mmXvhzdKZJVUg0YVaoNOsUwEc6s30=";
  };
  pname = "mls-test-cli";
  version = "0.15.0";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-1.0.0" = "sha256-a3w/ZoIedcSmJLYvpo7pkCzxvPE9nwGx3owyj87h/Uo=";
    };
  };
  doCheck = false;
}
