{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "a1d354cea125c35232d4e490feb134ad060f03d5";
    sha256 = "sha256-7J2JuDh/D1itth533lfG1a6bF3bMapLvkTyu6TyywHk=";
  };
  pname = "mls-test-cli";
  version = "0.10.3";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-nyIMAlTy7CTV0bVQ0ytamKHpERgtsVKTX4zv7aHzemo=";
      "safe_pqc_kyber-0.6.2" = "sha256-9t+IIohCJcMIWRtqLA0idyMmjev82BtpST15Tthlge4=";
      "tls_codec-0.4.0" = "sha256-2wCreWSfduxjUyfkGkXWqawLxZ2yb167msjv5PdGEpw=";
    };
  };
  doCheck = false;
}
