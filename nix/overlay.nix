let
  staticBinaryInTarball = { stdenv, fetchurl, pname, version, linuxAmd64Url, linuxAmd64Sha256, darwinAmd64Url, darwinAmd64Sha256, binPath ? pname }:
    stdenv.mkDerivation {
      inherit pname version;

      src =
        if stdenv.isDarwin
        then
          fetchurl
            {
              url = darwinAmd64Url;
              sha256 = darwinAmd64Sha256;
            }
        else
          fetchurl {
            url = linuxAmd64Url;
            sha256 = linuxAmd64Sha256;
          };

      installPhase = ''
        mkdir -p $out/bin
        cp ${binPath} $out/bin
      '';
    };

  staticBinary = { stdenv, fetchurl, pname, version, linuxAmd64Url, linuxAmd64Sha256, darwinAmd64Url, darwinAmd64Sha256, binPath ? pname }:
    stdenv.mkDerivation {
      inherit pname version;

      src =
        if stdenv.isDarwin
        then
          fetchurl
            {
              url = darwinAmd64Url;
              sha256 = darwinAmd64Sha256;
            }
        else
          fetchurl {
            url = linuxAmd64Url;
            sha256 = linuxAmd64Sha256;
          };
      phases = [ "installPhase" "patchPhase" ];

      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/${binPath}
        chmod +x $out/bin/${binPath}
      '';
    };

  sources = import ./sources.nix;
in

self: super: {
  # use crate2nix from our pin, the version in our nixpkgs is too old
  crate2nix = ((import sources.crate2nix) { pkgs = super; });

  cryptobox = self.callPackage ./pkgs/cryptobox { };
  zauth = self.callPackage ./pkgs/zauth { };
  mls-test-cli = self.callPackage ./pkgs/mls-test-cli { };

  # Named like this so cabal2nix can find it
  rusty_jwt_tools_ffi = self.callPackage ./pkgs/rusty_jwt_tools_ffi { };

  nginxModules = super.nginxModules // {
    zauth = {
      name = "zauth";
      src = ../services/nginz/third_party/nginx-zauth-module;
      inputs = [ self.pkg-config self.zauth.lib ];
      meta = {
        license = [ self.lib.licenses.agpl3Only ];
      };
    };
  };

  nginz = (super.nginx.overrideAttrs rec {
    version = "1.22.1";
    src = super.fetchurl {
      url = "https://nginx.org/download/nginx-${version}.tar.gz";
      hash = "sha256-nrszOp6CuVKs0+K0rrHU/2QG9ySRurbNn+afDepzfzE=";
    };
  }).override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };

  stack = staticBinaryInTarball rec {
    pname = "stack";
    version = "2.7.3";

    darwinAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v${version}/stack-${version}-osx-x86_64.tar.gz";
    darwinAmd64Sha256 = "0c7yx670h1qi2g5l4xx9s4552pz77k31lhjjd2rafi5g00501ra2";

    linuxAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v${version}/stack-${version}-linux-x86_64-static.tar.gz";
    linuxAmd64Sha256 = "sha256-xbziTe+isrhvG7sUvtTx7oO+wUxu2fzIEXTVRz+/NFA=";

    inherit (super) stdenv fetchurl;
  };

  kind = staticBinary {
    pname = "kind";
    version = "0.11.0";

    darwinAmd64Url = "https://github.com/kubernetes-sigs/kind/releases/download/v0.11.1/kind-darwin-amd64";
    darwinAmd64Sha256 = "432bef555a70e9360b44661c759658265b9eaaf7f75f1beec4c4d1e6bbf97ce3";

    linuxAmd64Url = "https://github.com/kubernetes-sigs/kind/releases/download/v0.11.1/kind-linux-amd64";
    linuxAmd64Sha256 = "949f81b3c30ca03a3d4effdecda04f100fa3edc07a28b19400f72ede7c5f0491";

    inherit (super) stdenv fetchurl;
  };

  rabbitmqadmin = super.callPackage ./pkgs/rabbitmqadmin { };
}
