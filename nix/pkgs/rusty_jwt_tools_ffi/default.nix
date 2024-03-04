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
    rev = "60424bf7031e2fa535aac658d0b5643624d19537";
    sha256 = "sha256-kdubK9FruZT8pbIwCHyAkxYj9yVM0q7ivNhNUNtNQCY=";
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
      "certval-0.1.4" = "sha256-gzkRC7/u/rARGPy3d37eBrAVml4XSDb6bRPpsESmttY=";
      "jwt-simple-0.12.1" = "sha256-5PAOwulL8j6f4Ycoa5Q+1dqEA24uN8rJt+i2RebL6eo=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
