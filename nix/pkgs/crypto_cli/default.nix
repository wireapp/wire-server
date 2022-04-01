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
    rev = "6d28cf49945b77932335d85264f61016b0ba49f2";
    sha256 = "sha256-8li6yaVhGf4O63NAVzYfNIzNODEwdtN/9gilG8EkdqE=";
  };
  doCheck = false;
  cargoPatches = [ ./crypto_cli.patch ];
  cargoSha256 = "sha256-ql6j3e6J0pI6gXiaGhjDkL09i1hg/PFEj6hhBIqzKbo=";
}

