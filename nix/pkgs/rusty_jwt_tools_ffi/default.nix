{ fetchFromGitHub
, lib
, rustPlatform
, pkg-config
, perl
, gitMinimal
}:

# TODO: update to crate2nix once https://github.com/wireapp/rusty-jwt-tools as a
# Cargo.lock file in its root (not at the ffi/ subpath).

let
  version = "0.9.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "05441e98d9c7c5ec9bfcfba84e885988278f10e6";
    sha256 = "sha256-HVq2BpPKp3cfdlKrS1AYWQ+a5VigFsYfSecZ60SFATI=";
  };
  cargoLockFile = builtins.toFile "cargo.lock" (builtins.readFile "${src}/Cargo.lock");

in
rustPlatform.buildRustPackage {
  name = "rusty_jwt-tools_ffi-${version}";
  inherit version src;

  cargoLock = {
    lockFile = cargoLockFile;
    outputHashes = {
      # if any of these need updating, replace / create new key with
      # lib.fakeSha256, rebuild, and replace with actual hash.
      "certval-0.1.4" = "sha256-4BWvSzFZhlA+mKj+Y6GNEwNSKikNGVjDoPxyxiw9TFE=";
      "biscuit-0.6.0-beta1" = "sha256-no7b4Un+7AES7EwWdZh/oeIa4w0caKLAUFsHWqgJOrg=";
      "jwt-simple-0.13.0" = "sha256-QkVi7EGrU3nF+/32tNjTtAILo8sjasR27nyRgBH+xoA=";
      "rcgen-0.9.2" = "sha256-3jFzInwdzFBot+L2Vm5NLF1ml33GH2+Iv3LqqGhLxFs=";
      "ring-0.17.0-not-released-yet" = "sha256-TP8yZo64J/d1fw8l2J4+ol70EcHvpvHJBdpF3A+6Dgo=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
