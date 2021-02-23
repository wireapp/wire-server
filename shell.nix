{ pkgs ? import ./nix }:
with pkgs;
let
  stack = stdenv.mkDerivation {
      pname = "stack";
      version = "2.3.1";

      src =
        if stdenv.isDarwin
        then fetchurl {
          url = "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-osx-x86_64.tar.gz";
          sha256 = "089nrb8mxf76a0r0hdccaxfvx1ly24b5zc0cy05gs4adybjygvkk";
        }
        else fetchurl {
          url = "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-static.tar.gz";
          sha256 = "0iqfqcd88rvlwgm2h8avs0rsi9f3pdxilvcacgrxskb1n8q8ibjb";
        };

      installPhase = ''
        mkdir -p $out/bin
        cp stack $out/bin
      '';
};
in mkShell {
  name = "shell";
  buildInputs = [
    docker-compose
    gnumake
    stack
    haskell-language-server
  ];
}
