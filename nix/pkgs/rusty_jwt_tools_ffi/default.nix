{ fetchFromGitHub
, lib
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  name = "rusty_jwt-tools_ffi-${version}";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    # if you update this, please generate a new Cargo.lock file es described below at `cargoPatches`
    rev = "6370cd556f03f6834d0b8043615ffaf0044ef1fa";
    sha256 = "sha256-vnTvKITie4pu+ISIl/RdYPfb/yWCdCI9eHl1KcZb050=";
  };
  cargoBuildFeatures = "haskell";
  cargoSha256 = "sha256-9etHOl3B/ybKdKMRUDb/VPxg4ghlIe75smWuupLORU8=";
  cargoPatches = [
    # a patch file to add/update Cargo.lock in the source code
    # it's good practice not to add Cargo.lock to the source code for libraries
    # see: https://doc.rust-lang.org/cargo/faq.html#why-do-binaries-have-cargolock-in-version-control-but-not-libraries
    # however, because it is required by nix, we need to manually generate and add it:
    # - `git clone git@github.com:wireapp/rusty-jwt-tools.git`
    # - checkout the commit specified in `rev`
    # - create a new branch: `git checkout -b patch-cargo-lock-<rev>` (replace `<rev>` with the commit hash)
    # - `cargo build --release --features haskell`
    # - `git add -f Cargo.lock`
    # - `git commit -am "generate new cargo.lock"`
    # - `git format-patch main`
    # - copy contents of `0001-generate-new-cargo.lock.patch` into `nix/pkgs/rusty_jwt_tools/add-Cargo.lock.patch`
    ./add-Cargo.lock.patch
  ];
}
