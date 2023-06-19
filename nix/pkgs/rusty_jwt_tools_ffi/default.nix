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
    rev = "fc4569c5b84d00a5cc8fc77b450714a5261cd3d9";
    sha256 = "sha256-cZffVKfH0FzA4Eo7YVxivT3JWTwz9uu1HWhPVlvbYqM=";
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
      "jwt-simple-0.11.4" = "sha256-zLKEvL6M7WD7F7HIABqq4b2rmlCS88QXDsj4JhAPe7o=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
