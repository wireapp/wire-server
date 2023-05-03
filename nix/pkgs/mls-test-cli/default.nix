{ fetchFromGitHub
, lib
, libsodium
, perl
, pkg-config
, rustPlatform
, stdenv
, gitMinimal
}:

let
  version = "0.7.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    rev = "29109bd32cedae64bdd9a47ef373710fad477590";
    sha256 = "sha256-1GMiEMkzcKPOd5AsQkQTSMLDkNqy3yjCC03K20vyFVY=";
  };
  cargoLockFile = builtins.toFile "cargo.lock" (builtins.readFile "${src}/Cargo.lock");
in rustPlatform.buildRustPackage rec {
  name = "mls-test-cli-${version}";
  inherit version src;

  cargoLock = {
    lockFile = cargoLockFile;
    outputHashes = {
      "hpke-0.10.0" = "sha256-XYkG72ZeQ3nM4JjgNU5Fe0HqNGkBGcI70rE1Kbz/6vs=";
      "openmls-0.20.0" = "sha256-i5xNTYP1wPzwlnqz+yPu8apKCibRZacz4OV5VVZwY5Y=";
    };
  };

  postPatch = ''
    cp ${cargoLockFile} Cargo.lock
  '';
  doCheck = false;
}
