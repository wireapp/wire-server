{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "0b7bad3a5021d069bcf02aa0d0a3fe0a6fdabe72";
    sha256 = "sha256-bFNqDG2UhN8kOEdGFdhPHN/Wz1y67Wcp1c/z0f0vHfE=";
  };
  pname = "mls-test-cli";
  version = "0.11";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "hpke-0.10.0" = "sha256-T1+BFwX6allljNZ/8T3mrWhOejnUU27BiWQetqU+0fY=";
      "openmls-1.0.0" = "sha256-MOf6F6jy2ofZ05leN9npDAlxYkn2S+hVOq/MSlKWBiU=";
      "safe_pqc_kyber-0.6.2" = "sha256-9t+IIohCJcMIWRtqLA0idyMmjev82BtpST15Tthlge4=";
    };
  };
  doCheck = false;
}
