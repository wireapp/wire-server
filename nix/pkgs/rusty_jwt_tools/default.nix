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
    rev = "6370cd556f03f6834d0b8043615ffaf0044ef1fa";
    sha256 = "sha256-vnTvKITie4pu+ISIl/RdYPfb/yWCdCI9eHl1KcZb050=";
  };
  cargoSha256 = "sha256-gR9XKcJslCcXo3EnD3MweWm+pQr4/EAEuFyOq3/l97g=";
  cargoPatches = [
    # a patch file to add/update Cargo.lock in the source code
    ./add-Cargo.lock.patch
  ];
}
