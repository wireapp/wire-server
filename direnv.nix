{ pkgs ? import ./nix }:
let
  staticBinaryInTarball = { pname, version, linuxAmd64Url, linuxAmd64Sha256, darwinAmd64Url, darwinAmd64Sha256, binPath ? pname }:
    pkgs.stdenv.mkDerivation {
      inherit pname version;

      src =
        if pkgs.stdenv.isDarwin
        then pkgs.fetchurl {
          url = darwinAmd64Url;
          sha256 = darwinAmd64Sha256;
        }
        else pkgs.fetchurl {
          url = linuxAmd64Url;
          sha256 = linuxAmd64Sha256;
        };

      installPhase = ''
        mkdir -p $out/bin
        cp ${binPath} $out/bin
      '';
    };

    staticBinary = { pname, version, linuxAmd64Url, linuxAmd64Sha256, darwinAmd64Url, darwinAmd64Sha256, binPath ? pname }:
      pkgs.stdenv.mkDerivation {
        inherit pname version;

        src =
          if pkgs.stdenv.isDarwin
          then pkgs.fetchurl {
            url = darwinAmd64Url;
            sha256 = darwinAmd64Sha256;
          }
          else pkgs.fetchurl {
            url = linuxAmd64Url;
            sha256 = linuxAmd64Sha256;
          };
        phases = ["installPhase" "patchPhase"];

        installPhase = ''
          mkdir -p $out/bin
          cp $src $out/bin/${binPath}
          chmod +x $out/bin/${binPath}
        '';
      };

  pinned = {
    stack = staticBinaryInTarball {
      pname = "stack";
      version = "2.3.1";

      darwinAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-osx-x86_64.tar.gz";
      darwinAmd64Sha256 = "089nrb8mxf76a0r0hdccaxfvx1ly24b5zc0cy05gs4adybjygvkk";

      linuxAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-static.tar.gz";
      linuxAmd64Sha256 = "0iqfqcd88rvlwgm2h8avs0rsi9f3pdxilvcacgrxskb1n8q8ibjb";
    };

    helm = staticBinaryInTarball {
      pname = "helm";
      version = "3.6.3";

      darwinAmd64Url = "https://get.helm.sh/helm-v3.6.3-darwin-amd64.tar.gz";
      darwinAmd64Sha256 = "0djjvgla8cw27h8s4y6jby19f74j58byb2vfv590cd03vlbzz8c4";

      linuxAmd64Url = "https://get.helm.sh/helm-v3.6.3-linux-amd64.tar.gz";
      linuxAmd64Sha256 = "0qp28fq137b07haz4vsdbc5biagh60dcs29jj70ksqi5k6201h87";
    };

    helmfile = staticBinary {
      pname = "helmfile";
      version = "0.141.0";

      darwinAmd64Url = "https://github.com/roboll/helmfile/releases/download/v0.141.0/helmfile_darwin_amd64";
      darwinAmd64Sha256 = "0szfd3vy6fzd5657079hz5vii86f9xkg3bdzp3g4knkcw5x1kpxy";

      linuxAmd64Url = "https://github.com/roboll/helmfile/releases/download/v0.141.0/helmfile_linux_amd64";
      linuxAmd64Sha256 = "0f5d9w3qjvwip4qn79hsigwp8nbjpj58p289hww503j43wjyxx8r";
    };

    kubectl = staticBinaryInTarball {
      pname = "kubectl";
      version = "1.19.8";

      darwinAmd64Url = "https://dl.k8s.io/v1.19.8/kubernetes-client-darwin-amd64.tar.gz";
      darwinAmd64Sha256 = "23b847bb8b545c748e9078e7660c654eef74d15ccab8696d294f3d6c619c788e";

      linuxAmd64Url = "https://dl.k8s.io/v1.19.8/kubernetes-client-linux-amd64.tar.gz";
      linuxAmd64Sha256 = "8388ff8b5c676bdbb8fe07ef7077de937b0bf60154f302df5f248f38f95122aa";

      binPath = "client/bin/kubectl";
    };

    kind = staticBinary {
      pname = "kind";
      version = "0.11.0";

      darwinAmd64Url = "https://github.com/kubernetes-sigs/kind/releases/download/v0.11.1/kind-darwin-amd64";
      darwinAmd64Sha256 = "432bef555a70e9360b44661c759658265b9eaaf7f75f1beec4c4d1e6bbf97ce3";

      linuxAmd64Url = "https://github.com/kubernetes-sigs/kind/releases/download/v0.11.1/kind-linux-amd64";
      linuxAmd64Sha256 = "949f81b3c30ca03a3d4effdecda04f100fa3edc07a28b19400f72ede7c5f0491";
    };
  };
in pkgs.buildEnv {
  name = "wire-server-direnv";
  paths = [
    pkgs.docker-compose
    pkgs.gnumake
    pkgs.haskell-language-server
    pkgs.telepresence
    pkgs.jq
    pkgs.grpcurl
    pkgs.wget
    pkgs.cfssl
    pkgs.yq

    # To actually run buildah on nixos, I had to follow this: https://gist.github.com/alexhrescale/474d55635154e6b2cd6362c3bb403faf
    pkgs.buildah

    pinned.stack
    pinned.helm
    pinned.helmfile
    pinned.kubectl
    pinned.kind
  ];
}

