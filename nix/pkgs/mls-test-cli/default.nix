{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "7d97772f52f8dfaae79d4c73de2e32b551d2d0bf";
    sha256 = "sha256-OU/bEz98Gh/HPVymGpoQ2K4ccpTZIV0xwsgEEnakOws=";
  };
  pname = "mls-test-cli";
  version = "0.10.2";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-nyIMAlTy7CTV0bVQ0ytamKHpERgtsVKTX4zv7aHzemo=";
      "safe_pqc_kyber-0.6.2" = "sha256-9t+IIohCJcMIWRtqLA0idyMmjev82BtpST15Tthlge4=";
      "tls_codec-0.3.0" = "sha256-IO6tenXKkC14EoUDp/+DtFNOVzDfOlLu8K1EJI7sOzs=";
    };
  };
  doCheck = false;
}
