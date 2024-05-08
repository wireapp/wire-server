{ fetchFromGitHub
, rustPlatform
}:
# TODO: migrate to crate2nix once
# https://github.com/nix-community/crate2nix/issues/310 is fixed
rustPlatform.buildRustPackage rec {
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "c7c416f533417858ff2882dbb5b29f7c090b0470";
    sha256 = "sha256-80k166n7MW0DCtnQ9z0hNgKb9e/nng3aYtSIvoN+Phc=";
  };
  pname = "mls-test-cli";
  version = "0.11";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "hpke-0.11.0" = "sha256-58uUnXma50AecSdg+DfT1xkaDimrT53dPmw8M4EIwh8=";
      "openmls-1.0.0" = "sha256-iRiUbDZMNf43itWiNascNBscfaIZdwcDdwhJPwYw8Uk=";
      "safe_pqc_kyber-0.6.2" = "sha256-9t+IIohCJcMIWRtqLA0idyMmjev82BtpST15Tthlge4=";
    };
  };
  doCheck = false;
}
