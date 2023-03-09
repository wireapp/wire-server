{ fetchFromGitHub
, lib
, rustPlatform
, pkg-config
, perl
, gitMinimal
}:

rustPlatform.buildRustPackage rec {
  name = "rusty_jwt-tools_ffi-${version}";
  version = "0.2.0";
  nativeBuildInputs = [ pkg-config perl gitMinimal ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "rusty-jwt-tools";
    # if you update this, please generate a new Cargo.lock file es described below at `cargoPatches`
    rev = "c213978cf0552237dc71e255a83110b18ee95972";
    sha256 = "sha256-7jVKReJEXKui6BOJZ3Cm2ykcAVVaSXgKhsZuu3wJoH0=";
  };
  doCheck = false;
  cargoSha256 = "sha256-ylvsbpgwPfENz0TP0uM9kYLyx7NjQbUv9cpT5het+xE=";
  cargoDepsHook = ''
    mkdir -p rusty_jwt-tools_ffi-${version}-vendor.tar.gz/ring/.git
  '';
  cargoPatches = [
    # a patch file to add/update Cargo.lock in the source code
    # it's good practice not to add Cargo.lock to the source code for libraries
    # see: https://doc.rust-lang.org/cargo/faq.html#why-do-binaries-have-cargolock-in-version-control-but-not-libraries
    # however, because it is required by nix, we need to manually generate and add it:
    # - `git clone git@github.com:wireapp/rusty-jwt-tools.git`
    # - checkout the commit specified in `rev`
    # - create a new branch: `git checkout -b patch-cargo-lock-<rev>` (replace `<rev>` with the commit hash)
    # - `cargo clean && cargo build --release`
    # - `git add -f Cargo.lock`
    # - `git commit -am "generate new cargo.lock"`
    # - `git format-patch main`
    # - copy contents of `0001-generate-new-cargo.lock.patch` into `nix/pkgs/rusty_jwt_tools/add-Cargo.lock.patch`
    ./add-Cargo.lock.patch
  ];
}
