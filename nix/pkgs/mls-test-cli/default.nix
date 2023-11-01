{ fetchFromGitHub
, rustPlatform
}:

# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
let
  version = "0.7.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "e6e6ce0c29f0e48e84b4ccef058130aca0625492";
    sha256 = "sha256-J9M8w3GJnULH3spKEuPGCL/t43zb2Wd+YfZ0LY3YITo=";
  };
  cargoLockFile = builtins.toFile "cargo.lock" (builtins.readFile "${src}/Cargo.lock");
in
rustPlatform.buildRustPackage rec {
  name = "mls-test-cli-${version}";
  inherit version src;

  cargoLock = {
    lockFile = cargoLockFile;
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-s1ejM/aicFGvsKY7ajEun1Mc645/k8QVrE8YSbyD3Fg=";
      "safe_pqc_kyber-0.6.0" = "sha256-Ch1LA+by+ezf5RV0LDSQGC1o+IWKXk8IPvkwSrAos68=";
      "tls_codec-0.3.0" = "sha256-IO6tenXKkC14EoUDp/+DtFNOVzDfOlLu8K1EJI7sOzs=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
