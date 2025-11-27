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

  nginz = super.nginx.override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };

  rabbitmqadmin = super.callPackage ./pkgs/rabbitmqadmin { };

  sbomqs = super.callPackage ./pkgs/sbomqs { };
}
