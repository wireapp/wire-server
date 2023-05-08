{ fetchFromGitHub
, lib
, rustPlatform
, pkg-config
, perl
, gitMinimal
}:

let
  version = "0.3.4";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "v${version}";
    sha256 = "sha256-awfpyMmDGWLViKI8Pr/BjbfnmFKo4JAcUB0+o6/prOA=";
  };
  cargoLockFile = builtins.toFile "cargo.lock" (builtins.readFile "${src}/ffi/Cargo.lock");

in
rustPlatform.buildRustPackage {
  name = "rusty_jwt-tools_ffi-${version}";
  inherit version src;

  cargoLock = {
    lockFile = cargoLockFile;
    outputHashes = {
      # if any of these need updating, replace / create new key with
      # lib.fakeSha256, rebuild, and replace with actual hash.
      "biscuit-0.6.0-beta1" = "sha256-j8Pxi2nHgsKz6umroYjwR8sr1xLQAaWdnej5U9+L5ko=";
      "jwt-simple-0.11.3" = "sha256-kVBTXYtBW9SE6F6nmH71iVc0KKxvpX/axCvMAP1cZvY=";
      "ring-0.17.0-not-released-yet" = "sha256-9M4lR68r8phscSFw9Xh+CVHnOkilDI0brAdU0tW3xaA=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
