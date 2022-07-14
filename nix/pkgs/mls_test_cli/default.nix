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
    rev = "6f5c0a871dac63e4a656fc5b8fd72875bb023b4b";
    sha256 = "sha256:1ah72kbhakp0sqkjark20912yf8ain3yjj5f76sa3vzqgfmvwahq";
  };
  doCheck = false;
  cargoSha256 = "sha256:1cwyzn5gwzdb92k1b02yzpj1mv9x8vnx329v54qsm089nq5drp11";
}
