with (import <nixpkgs> {});
let
  pkgs = import ./nix;
  native_libs = lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

in
pkgs.haskell.lib.buildStackProject {
  name = "wire-server";
  nativeBuildInputs = native_libs;
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
  ghc = pkgs.haskell.compiler.ghc8103;
}
