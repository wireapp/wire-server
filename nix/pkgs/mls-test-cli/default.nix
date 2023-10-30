{ fetchFromGitHub
, libsodium
, perl
, pkg-config
, rustPlatform
, gitMinimal
}:

let
  version = "0.7.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "baaa5c78411a5bf6d697803276b991523c111631";
    sha256 = "sha256-M6bWB5hWl+WSblcH6L+AyGD+7ef9TvRs8wKYq7lJyS8=";
  };
  cargoLockFile = builtins.toFile "cargo.lock" (builtins.readFile "${src}/Cargo.lock");
in rustPlatform.buildRustPackage rec {
  name = "mls-test-cli-${version}";
  inherit version src;

  cargoLock = {
    lockFile = cargoLockFile;
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-tAIm8+IgubNnU2M2A5cxHY5caiEQmisw73I9/cqfvUc=";
      "safe_pqc_kyber-0.6.0" = "sha256-Ch1LA+by+ezf5RV0LDSQGC1o+IWKXk8IPvkwSrAos68=";
      "tls_codec-0.3.0" = "sha256-IO6tenXKkC14EoUDp/+DtFNOVzDfOlLu8K1EJI7sOzs=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
