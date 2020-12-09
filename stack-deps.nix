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
    ncurses
    openssl
    pkgconfig
    protobuf
    pcre
    snappy
    zlib
    lzma
  ];
  ghc = pkgs.haskell.compiler.ghc884;
}
