{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "c560a5a0c3e89b4cfafd07bbd4f2fb42c22afa28";
    sha256 = "sha256-QuCXrrLOgnPXybJBai4hVX6CdAsOzgbOEcBUp6N2mfE=";
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
