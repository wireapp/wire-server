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
    # https://github.com/wireapp/rusty-jwt-tools/pull/52
    rev = "2d947e3a62d58ea5a3f0c36a6ba0a28d56bd4ba6";
    sha256 = "sha256-bADW6u4v86M6WwAIfMmXbHfTiAZEAmFxLxO2izj4xV8=";
  };
  doCheck = false;
  cargoSha256 = "sha256-JdNBBzRrTlTno0PWf2B0TpltdQt4YqF2KRI/WghGsl4=";
  cargoDepsHook = ''
    mkdir -p rusty_jwt-tools_ffi-${version}-vendor.tar.gz/ring/.git
  '';
}
