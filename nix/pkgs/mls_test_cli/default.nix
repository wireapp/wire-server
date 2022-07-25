{ fetchFromGitHub
, lib
, libsodium
, perl
, pkg-config
, rustPlatform
, stdenv
}:

rustPlatform.buildRustPackage rec {
  name = "mls-test-cli-${version}";
  version = "0.2.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    sha256 = "sha256:0f3d9dhnxf0nx9vz9hrfhx1lkash5891wxz2163widw5nra66cvi";
    rev = "14963eb639847f9ab9ff62c0660d02899935e776";
  };
  doCheck = false;
  cargoSha256 = "sha256:1cwyzn5gwzdb92k1b02yzpj1mv9x8vnx329v54qsm089nq5drp11";
}
