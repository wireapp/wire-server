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
    rev = "87845faa7d5ee69652747ceaf1664baa8198c0d8";
    sha256 = "sha256-DoQ6brp1KvglVVCDp4vC5zaRx76IUywu3Rcu/TzJlvo=";
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
