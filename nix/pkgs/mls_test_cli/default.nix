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
    rev = "ed689dda175f4a11a657ed7d9474bd391023188b";
    sha256 = "sha256:043nc7nqjs71i1hbjpygv10mw9axq0xw0y5ncm1gprw6cvnyimlq";
  };
  doCheck = false;
  cargoSha256 = "sha256:1cwyzn5gwzdb92k1b02yzpj1mv9x8vnx329v54qsm089nq5drp11";
}
