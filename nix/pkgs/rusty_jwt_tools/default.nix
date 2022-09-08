{ fetchFromGitHub
, lib
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  name = "rusty-jwt-tools-${version}";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "6e6e741c3e44270cc08bf625504dab7e418ef3e9";
    sha256 = "sha256-xOlN9+kqDa94b1UzpUrr6WvmrUkqWO1weze5Gsu7KaQ=";
  };
  cargoSha256 = "sha256-kx2SQ61sVKo6PW54nGIeSSXgdoCt0UmYZ+2bIQ0n1sE=";
  cargoPatches = [
    # a patch file to add/update Cargo.lock in the source code
    ./add-Cargo.lock.patch
  ];
}
