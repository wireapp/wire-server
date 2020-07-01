let
  pkgs = import <nixpkgs>;
in
pkgs.haskell.lib.buildStackProject {
  name = "wire-server";
  buildInputs = with pkgs; [
    pkgconfig
    zlib
  ];
  ghc = pkgs.haskell.compiler.ghc865;
}
