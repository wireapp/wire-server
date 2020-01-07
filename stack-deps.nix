let
  # Pin nixpkgs for all dependencies.
  # If you want to update.
  # 1. go to https://nixos.org/channels/nixos-19.09
  # 2. copy the URL to nixexprs.tar.gz and the sha256 hash
  # 3. Uncomment the  sha256 = 00000 field
  # 4. nix-build
  # 5. Make nix complain to you what the correct hash is.
  # 6. comment sha256 = 0000 and   add sha256 = <actual-hash>
  # 7. nix-build
  # 8. commit
  # TODO(arianvp): There are tools that automate this; we should use them
  pkgsTar = builtins.fetchTarball {
    name = "nixos-1909";
    url = "https://releases.nixos.org/nixos/19.09/nixos-19.09.1019.c5aabb0d603/nixexprs.tar.xz";
    sha256 = "1hjw843g964aj9cd9p6x5473yy4sfmqnqlvavc5c1lbqa8v676zg";
    # sha256 = "0000000000000000000000000000000000000000000000000000";
  };
  pkgs = import pkgsTar {};
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
      cryptobox-c
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
    ghc = pkgs.haskell.compiler.ghc844;
  }
