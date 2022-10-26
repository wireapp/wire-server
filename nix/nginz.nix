{ nginxModules
, pkg-config
, callPackage
, stdenv
, symlinkJoin
, dockerTools
, writeTextDir
, runCommand
, gnugrep
, coreutils
, nginx
, inotify-tools
, dumb-init
, cacert
, bashInteractive
, openssl
, lib
, makeWrapper
, writers
}:
let zauth = callPackage ./pkgs/zauth { };

    nginzModules = nginxModules // {
      zauth = {
        src = ../services/nginz/third_party/nginx-zauth-module;
        inputs = [ pkg-config zauth ];
      };
    };

    nginz = nginx.override {
      # openssl = (openssl.overrideAttrs (oldAttrs: {
      #   patches = oldAttrs.patches or [] ++ [
      #     ./patch.patch
      #   ];
      # }));
      modules = [
        nginzModules.vts
        nginzModules.moreheaders
        nginzModules.zauth
      ];
    };

    reload-script = stdenv.mkDerivation {
      name = "reload-script";
      src = (writers.writeBash "nginz_reload.sh" ../services/nginz/nginz_reload.sh);
      phases = "installPhase";
      nativeBuildInputs = [ makeWrapper ];
      installPhase = ''
         mkdir -p $out/bin
         cp $src $out/bin/nginz_reload.sh
         wrapProgram $out/bin/nginz_reload.sh \
           --prefix PATH : "${lib.makeBinPath [ inotify-tools nginz ]}"
      '';
    };

    # copied from nixpkgs fakeNss, but using nginx as username
    nginxFakeNss =  symlinkJoin {
      name = "fake-nss";
      paths = [
        (writeTextDir "etc/passwd" ''
          root:x:0:0:root user:/var/empty:/bin/sh
          nginx:x:101:101:nginx:/var/empty:/bin/sh
          nobody:x:65534:65534:nobody:/var/empty:/bin/sh
        '')
        (writeTextDir "etc/group" ''
          root:x:0:
          nginx:x:101:
          nobody:x:65534:
        '')
        (writeTextDir "etc/nsswitch.conf" ''
          hosts: files dns
        '')
        (runCommand "var-empty" { } ''
          mkdir -p $out/var/empty
        '')
        # it seems nginx still tries to log, and doesn't create
        # these directories automatically
        (runCommand "nginx-misc" { } ''
          mkdir -p $out/var/log/nginx
          mkdir -p $out/var/cache/nginx
        '')
      ];
    };

    nginzImage = dockerTools.buildLayeredImage {
      name = "quay.io/wire/nginz";
      maxLayers = 5;
      contents = [
        cacert
        bashInteractive
        gnugrep
        coreutils
        nginxFakeNss
      ];
      config = {
        Entrypoint = ["${dumb-init}/bin/dumb-init" "--" "${reload-script}/bin/nginz_reload.sh" "-g" "daemon off;" "-c" "/etc/wire/nginz/conf/nginx.conf"];
        Env = ["SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"];
        ExposedPorts = {
          "80/tcp" = {};
        };
      };
    };
in
  nginzImage
