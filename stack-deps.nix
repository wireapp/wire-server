let
  pkgs = import ./nix;
  native_libs = pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
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
  ghc = pkgs.haskell.compiler.ghc884;

  # This is required as the environment variables exported before running stack
  # do not make it into the shell in which stack runs test.
  HSPEC_OPTIONS = "--fail-on-focused";
}
