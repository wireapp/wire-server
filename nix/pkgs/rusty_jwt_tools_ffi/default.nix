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
  version = "0.8.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "064d531b6f0d0b502755dceb3ab73f0f9ad02143";
    sha256 = "sha256-OqL4ue6swci3JqQKNmzcvpGxQAMhF8bHTXMp6dvIn9o=";
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
      "biscuit-0.6.0-beta1" = "sha256-no7b4Un+7AES7EwWdZh/oeIa4w0caKLAUFsHWqgJOrg=";
      "certval-0.1.4" = "sha256-mUg3Kx1I/r9zBoB7tDaZsykFkE+tsN+Rem6DjUOZbuU=";
      "jwt-simple-0.12.1" = "sha256-5PAOwulL8j6f4Ycoa5Q+1dqEA24uN8rJt+i2RebL6eo=";
      "rcgen-0.9.2" = "sha256-3jFzInwdzFBot+L2Vm5NLF1ml33GH2+Iv3LqqGhLxFs=";
      "ring-0.17.0-not-released-yet" = "sha256-TP8yZo64J/d1fw8l2J4+ol70EcHvpvHJBdpF3A+6Dgo=";
      "x509-ocsp-0.2.1" = "sha256-Tdswn977QtS+i69q82dF/nkXIblUaCsqPD2SqUIYLWc=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
