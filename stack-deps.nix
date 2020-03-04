let
  pkgs = import ./nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "wire-server";
  buildInputs = with pkgs; [
    cryptobox
    geoip
    git
    icu
    libsodium
    libxml2
    openssl
    pkgconfig
    protobuf
    snappy
    zlib
  ];
  ghc = pkgs.haskell.compiler.ghc865;
}
