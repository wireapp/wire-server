{ fetchFromGitHub
, libsodium
, perl
, pkg-config
, rustPlatform
, gitMinimal
}:

rustPlatform.buildRustPackage rec {
  pname = "mls-test-cli";
  version = "0.6.0";
  nativeBuildInputs = [ pkg-config perl gitMinimal ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "mls-test-cli";
    sha256 = "sha256-/XQ/9oQTPkRqgMzDGRm+Oh9jgkdeDM1vRJ6/wEf2+bY=";
    rev = "c6f80be2839ac1ed2894e96044541d1c3cf6ecdf";
  };
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "openmls-0.4.1" = "sha256-oEPziXyGmPV6C80lQpi0z7Ehl3/mGFz0HaePT8h3y0Q=";
      "ring-0.17.0-not-released-yet" = "sha256-n8KuJRcOdMduPTjDBU1n1eec3w9Eat/8czvGRTGbqsI=";
      "x509-parser-0.13.1" = "sha256-ipHZm3MmiOssGkFC5O4h/Y3p1U0aj7wu+LGaBuQImuU=";
    };
  };
  doCheck = false;
  preBuild = ''
    mkdir $CARGO_HOME/cargo-vendor-dir/ring-0.17.0-not-released-yet/.git
  '';
}
