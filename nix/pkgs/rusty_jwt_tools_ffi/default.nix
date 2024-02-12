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
  version = "0.8.5";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "99acb427b2169d726f356d30dec55eae83dda6b6";
    sha256 = "sha256-x1W79spOZeFHabRbhMksz6gLtRIpl2E7WCiXuzIMoFM=";
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
      "certval-0.1.4" = "sha256-mUg3Kx1I/r9zBoB7tDaZsykFkE+tsN+Rem6DjUOZbuU=";
      "jwt-simple-0.12.1" = "sha256-5PAOwulL8j6f4Ycoa5Q+1dqEA24uN8rJt+i2RebL6eo=";
      "x509-ocsp-0.2.1" = "sha256-o+r9h0CcexWqJIIoZdOgSd7hWIb91BheW6UZI98RpLA=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
