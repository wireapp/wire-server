{ pkgs ? import <nixpkgs> {} }:
let 
  cryptobox-c = pkgs.callPackage ({fetchFromGitHub, rustPlatform,  pkgconfig, libsodium}:
    rustPlatform.buildRustPackage rec {
      name = "cryptobox-c-${version}";
      version = "2019-06-17";
      buildInputs = [ pkgconfig libsodium ];
      src = fetchFromGitHub {
        owner = "wireapp";
        repo = "cryptobox-c";
        rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
        sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
      };
      cargoSha256 = "1373rpy0fi3cpacv06x1cv4cv0brwdri2680ymdkq8w44syp20ym";
      postInstall = ''
        mkdir -p $out/include
        cp src/cbox.h $out/include
      '';
    }) {};
in
  pkgs.haskell.lib.buildStackProject {
    name = "wire-server";
    buildInputs = with pkgs; [ 
      pkgconfig
      cryptobox-c 
      libsodium
      geoip
      protobuf
      openssl
      snappy
      icu
      zlib
      libxml2
    ];
    ghc = pkgs.haskell.compiler.ghc844;
  }
