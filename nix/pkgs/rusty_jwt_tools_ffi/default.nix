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
  version = "0.5.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "6704e08376bb49168133d8f4ce66155adeb6bfb0";
    sha256 = "sha256-ocmeFXjU3psCO+hpDuEAIzYIm4QzP+jHJR/V8yyw6Lw=";
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
