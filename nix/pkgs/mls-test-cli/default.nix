{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "30417661eb4e006972ffbdbac80fce6ec64fa90e";
    sha256 = "sha256-VYSzPejf+Oc5goyxOqlZTwbPj6kqGM6eotefH4eskKE=";
  };
  pname = "mls-test-cli";
  version = "0.9.1";
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
