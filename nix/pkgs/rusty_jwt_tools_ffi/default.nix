{ fetchFromGitHub
, lib
, rustPlatform
, pkg-config
, perl
, gitMinimal
}:

let
  version = "0.14.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    rev = "b5de03a239d41b51e7cf9cd7c2674fa6be205134";
    sha256 = "sha256-k7zxtdpLYWP+EYa+Vf6enViS+jIR2zt07yBZdozhKuE=";
  };
in
rustPlatform.buildRustPackage {
  name = "rusty_jwt-tools_ffi-${version}";
  inherit version src;
  # `buildRustPackage` requires `cargoHash`. So we have to update it as well
    # when the Git `rev` is changed.
  cargoHash = "sha256-gvFEwb+Cxnk7GhWrKs4hEhfyAI/QiE8Zqd2ZXXLbvuE=";
}
