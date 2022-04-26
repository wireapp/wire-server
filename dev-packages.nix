{ pkgs ? import ./nix }:
let
  staticBinaryInTarball = { pname, version, linuxAmd64Url, linuxAmd64Sha256, darwinAmd64Url, darwinAmd64Sha256, binPath ? pname }:
    pkgs.stdenv.mkDerivation {
      inherit pname version;

      src =
        if pkgs.stdenv.isDarwin
        then
          pkgs.fetchurl
            {
              url = darwinAmd64Url;
              sha256 = darwinAmd64Sha256;
            }
        else
          pkgs.fetchurl {
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
        then
          pkgs.fetchurl
            {
              url = darwinAmd64Url;
              sha256 = darwinAmd64Sha256;
            }
        else
          pkgs.fetchurl {
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

  pinned = {
    stack = staticBinaryInTarball rec {
      pname = "stack";
      version = "2.7.3";

      darwinAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v${version}/stack-${version}-osx-x86_64.tar.gz";
      darwinAmd64Sha256 = "0c7yx670h1qi2g5l4xx9s4552pz77k31lhjjd2rafi5g00501ra2";

      linuxAmd64Url = "https://github.com/commercialhaskell/stack/releases/download/v${version}/stack-${version}-linux-x86_64-static.tar.gz";
      linuxAmd64Sha256 = "sha256-xbziTe+isrhvG7sUvtTx7oO+wUxu2fzIEXTVRz+/NFA=";
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

  c-lib-out-deps = [
    pkgs.cryptobox
    pkgs.icu.out
    pkgs.libsodium.out
    pkgs.libxml2.out
    pkgs.ncurses.out
    pkgs.openssl.out
    pkgs.pcre.out
    pkgs.snappy.out
    pkgs.zlib.out
    pkgs.lzma.out
  ];

  compile-deps = pkgs.buildEnv {
    name = "wire-server-compile-deps";
    paths = [
      pkgs.bash
      pkgs.coreutils
      pkgs.gnused
      pkgs.gnugrep
      pkgs.pkgconfig
      pkgs.gawk
      pkgs.git

      pkgs.haskell.compiler.ghc8107
      pkgs.protobuf

      pkgs.cryptobox
      pkgs.icu.dev
      pkgs.libsodium.dev
      pkgs.libxml2.dev
      pkgs.ncurses.dev
      pkgs.openssl.dev
      pkgs.pcre.dev
      pkgs.snappy.dev
      pkgs.zlib.dev
      pkgs.lzma.dev
    ] ++ c-lib-out-deps;
  };

  # This performs roughly the same setup as direnv's load_prefix function, but
  # only when invoking cabal. This means that we can set LD_LIBRARY_PATH just
  # for cabal, as setting it in direnv can interfere with programs in the host
  # system, especially for non-NixOS users.
  cabal-wrapper = pkgs.writeShellScriptBin "cabal" ''
    export CPATH="${compile-deps}/include"
    export LD_LIBRARY_PATH="${compile-deps}/lib"
    export LIBRARY_PATH="${compile-deps}/lib"
    export PKG_CONFIG_PATH="${compile-deps}/lib/pkgconfig"
    export PATH="${compile-deps}/bin"
    export CONFIG_SHELL="${compile-deps}/bin/sh"
    exec "${pkgs.cabal-install}/bin/cabal" "$@"
  '';

  # stack-deps.nix sets LD_LIBRARY_PATH, which could be incompatible with the
  # system bash. To ensure that nix-shell invoked by stack uses the correct
  # shell to build we set NIX_BUILD_SHELL here.
  stack-wrapper = pkgs.writeShellScriptBin "stack" ''
    export NIX_BUILD_SHELL="${pkgs.bash}/bin/bash"
    exec "${pinned.stack}/bin/stack" "$@"
  '';
in
[
  pkgs.cfssl
  pkgs.docker-compose
  pkgs.gnumake
  pkgs.gnused
  (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
  pkgs.jq
  pkgs.niv
  pkgs.ormolu
  pkgs.telepresence
  pkgs.wget
  pkgs.yq
  pkgs.rsync
  pkgs.netcat
  pkgs.crypto_cli

  # To actually run buildah on nixos, I had to follow this: https://gist.github.com/alexhrescale/474d55635154e6b2cd6362c3bb403faf
  pkgs.buildah

  stack-wrapper
  pinned.helm
  pinned.helmfile
  pinned.kubectl
  pinned.kind

  # For cabal-migration
  pkgs.haskellPackages.cabal-plan

  # We don't use pkgs.cabal-install here, as we invoke it with a wrapper
  # which sets LD_LIBRARY_PATH and others correctly.
  cabal-wrapper
  pkgs.haskellPackages.implicit-hie
]
++ c-lib-out-deps # Required to run HLS
