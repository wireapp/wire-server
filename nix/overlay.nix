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
          fetchurl {
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

in

self: super: {
  cryptobox = self.callPackage ./pkgs/cryptobox { };
  zauth = self.callPackage ./pkgs/zauth { };
  mls-test-cli = self.callPackage ./pkgs/mls-test-cli { };

  # Named like this so cabal2nix can find it
  rusty_jwt_tools_ffi = self.callPackage ./pkgs/rusty_jwt_tools_ffi { };

  nginxModules = super.nginxModules // {
    zauth = {
      src = ../services/nginz/third_party/nginx-zauth-module;
      inputs = [ self.pkg-config self.zauth ];
    };
  };

  nginz = super.nginx.override {
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

  helm = staticBinaryInTarball {
    pname = "helm";
    version = "3.6.3";

    darwinAmd64Url = "https://get.helm.sh/helm-v3.6.3-darwin-amd64.tar.gz";
    darwinAmd64Sha256 = "0djjvgla8cw27h8s4y6jby19f74j58byb2vfv590cd03vlbzz8c4";

    linuxAmd64Url = "https://get.helm.sh/helm-v3.6.3-linux-amd64.tar.gz";
    linuxAmd64Sha256 = "0qp28fq137b07haz4vsdbc5biagh60dcs29jj70ksqi5k6201h87";

    inherit (super) stdenv fetchurl;
  };

  helmfile = staticBinary {
    pname = "helmfile";
    version = "0.141.0";

    darwinAmd64Url = "https://github.com/roboll/helmfile/releases/download/v0.141.0/helmfile_darwin_amd64";
    darwinAmd64Sha256 = "0szfd3vy6fzd5657079hz5vii86f9xkg3bdzp3g4knkcw5x1kpxy";

    linuxAmd64Url = "https://github.com/roboll/helmfile/releases/download/v0.141.0/helmfile_linux_amd64";
    linuxAmd64Sha256 = "0f5d9w3qjvwip4qn79hsigwp8nbjpj58p289hww503j43wjyxx8r";

    inherit (super) stdenv fetchurl;
  };

  kubectl = staticBinaryInTarball {
    pname = "kubectl";
    version = "1.19.8";

    darwinAmd64Url = "https://dl.k8s.io/v1.19.8/kubernetes-client-darwin-amd64.tar.gz";
    darwinAmd64Sha256 = "23b847bb8b545c748e9078e7660c654eef74d15ccab8696d294f3d6c619c788e";

    linuxAmd64Url = "https://dl.k8s.io/v1.19.8/kubernetes-client-linux-amd64.tar.gz";
    linuxAmd64Sha256 = "8388ff8b5c676bdbb8fe07ef7077de937b0bf60154f302df5f248f38f95122aa";

    binPath = "client/bin/kubectl";

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

  # This is to match the ormolu version that ships with HLS.
  # This doesn't compile with ghc8107 howerver, so we use ghc92
  ormolu = super.haskell.lib.justStaticExecutables (super.haskell.lib.doJailbreak super.haskell.packages.ghc92.ormolu_0_5_0_1);
}
