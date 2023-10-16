{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "baaa5c78411a5bf6d697803276b991523c111631";
    sha256 = "sha256-M6bWB5hWl+WSblcH6L+AyGD+7ef9TvRs8wKYq7lJyS8=";
  };
  pname = "mls-test-cli";
  version = "0.9.0";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-tAIm8+IgubNnU2M2A5cxHY5caiEQmisw73I9/cqfvUc=";
      "safe_pqc_kyber-0.6.0" = "sha256-Ch1LA+by+ezf5RV0LDSQGC1o+IWKXk8IPvkwSrAos68=";
      "tls_codec-0.3.0" = "sha256-IO6tenXKkC14EoUDp/+DtFNOVzDfOlLu8K1EJI7sOzs=";
    };
  };
  doCheck = false;
}
