{ fetchFromGitHub
, lib
, libsodium
, perl
, pkg-config
, rustPlatform
, stdenv
}:

rustPlatform.buildRustPackage rec {
  name = "crypto-cli-${version}";
  version = "0.1.0";
  nativeBuildInputs = [ pkg-config perl ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "core-crypto";
    rev = "c63942e7a87b9d5a331109f472cb26f1efa147e9";
    sha256 = "sha256-RtlSAg9eGfLUmeYxLDHXmQ77E36vGnKyPaijcTj5Y3E=";
  };
  doCheck = false;
  cargoPatches = [ ./crypto_cli.patch ];
  cargoSha256 = "sha256-knBqluuw64NL5I3CQ8qnabu7jRy0+BL2RYR+ks3xhJI=";
}

