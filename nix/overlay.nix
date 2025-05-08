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

  pkgs_old = import sources.nixpkgs_old { config.allowUnfree = true; };
in

self: super: {

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
    version = "1.26.0";
    src = super.fetchurl {
      url = "https://nginx.org/download/nginx-${version}.tar.gz";
      hash = "sha256-0ubIQ51sbbUBXY6qskcKtSrvhae/NjGCh5l34IQ3BJc=";
    };
  }).override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };

  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      # Add overrides to haskellPackages here
    };
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

  rabbitmqadmin = super.callPackage ./pkgs/rabbitmqadmin { };

  sbomqs = super.callPackage ./pkgs/sbomqs { };

  # FUTUREWORK: Remove this override when vacuum-go has been fixed so it doesn't panic when running `make openapi-validate`
  vacuum-go = pkgs_old.vacuum-go;
}
