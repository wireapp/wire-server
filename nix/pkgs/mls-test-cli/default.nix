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
    rev = "f539bcc60ab3f7e2303742a37aa17b281b44bf3a";
    sha256 = "sha256-oyf+sot/aVnfoodecPGxTDxqNGk/KCX24LG7W9uP8mI=";
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
