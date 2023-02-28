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
    rev = "a68ed483f7e98613c0d5c3608c684f25225a58d3";
    sha256 = "sha256-+2fjwtG80l8Vt48QWKm4wevY7MQRAwuo4YFbjB+6w9I=";
  };
  doCheck = false;
  cargoSha256 = "sha256-BHq28U3OzYCPNmfnxlmXsz9XYEy1kRiNrFM9OTnAkk0=";
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
