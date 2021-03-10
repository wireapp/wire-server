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
      version = "3.1.1";

      darwinAmd64Url = "https://get.helm.sh/helm-v3.1.1-darwin-amd64.tar.gz";
      darwinAmd64Sha256 = "2ce00e6c44ba18fbcbec21c493476e919128710d480789bb35bd228ae695cd66";

      linuxAmd64Url = "https://get.helm.sh/helm-v3.1.1-linux-amd64.tar.gz";
      linuxAmd64Sha256 = "cdd7ad304e2615c583dde0ffb0cb38fc1336cd7ce8ff3b5f237434dcadb28c98";
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
  };
in pkgs.mkShell {
  name = "shell";
  buildInputs = [
    pkgs.docker-compose
    pkgs.gnumake
    pkgs.haskell-language-server
    pkgs.telepresence
    pkgs.jq
    pkgs.grpcurl

    pinned.stack
    pinned.helm
    pinned.kubectl
  ];
}
