{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "91ab11f67f6bd42de0df6f6cfd2a7498826735c6";
    sha256 = "sha256-ZrPPKORQ6K8uIU0Lz4ktAUpFTIh38MJuCJPcvsfoEzw=";
  };
  pname = "mls-test-cli";
  version = "0.14.0";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-1.0.0" = "sha256-a3w/ZoIedcSmJLYvpo7pkCzxvPE9nwGx3owyj87h/Uo=";
    };
  };
  doCheck = false;
}
