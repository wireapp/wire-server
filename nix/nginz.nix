{ nginxModules
, pkg-config
, callPackage
, stdenvNoCC
, dockerTools
, nginx
, inotify-tools
, dumb-init
, envsubst
, pkgs
, cacert
, coreutils
, bashInteractive
, openssl
, which
, gnugrep
, less
}:
let zauth = callPackage ./pkgs/zauth { };

    nginzModules = nginxModules // {
      zauth = {
        src = ../services/nginz/third_party/nginx-zauth-module;
        inputs = [ pkg-config zauth ];
      };
    };

    nginz = nginx.override {
      modules = [
        nginzModules.vts
        nginzModules.moreheaders
        nginzModules.zauth
      ];
    };

    reload-script = stdenvNoCC.mkDerivation {
      name = "reload-script";
      src = ../services/nginz/nginz_reload.sh;
      phases = "installPhase";
      installPhase = ''
         mkdir -p $out/usr/bin
         cp $src $out/usr/bin/nginz_reload.sh
         # pkgs.envsubst installs 'env' under /sbin/env, not what nginz_reload needs
         # so we add a symlink here.
         ln -s /sbin/env $out/usr/bin/env
      '';
    };

    # alpine-image = pkgs.dockerTools.pullImage {
    #   imageName = "alpine";
    #   imageDigest = "sha256:fe785cb65b7bcd332154183e4509ef9b6aeb9913d5b13fb6312df6ee9cc8a543";
    #   sha256 = "10jb3l46zz5vvpfddc24ppp0cibgdgkxi33n459dr5b7h0fhk51v";
    #   os = "linux";
    #   arch = "amd64";
    # };

    nginzImage = dockerTools.buildLayeredImage {
      # fromImage = "docker.io/alpine:3.15";
      name = "quay.io/wire/nginz";
      # maxLayers = 5;
      contents = [
        cacert
        coreutils
        bashInteractive
        openssl
        which
        gnugrep
        less
        dumb-init
        inotify-tools
        envsubst
        reload-script
        nginz
      ];
      extraCommands = ''
        # nginx still tries to read this directory even if error_log
        # directive is specifying another file :/
        mkdir -p var/log/nginx
        mkdir -p var/cache/nginx
      '';
      config = {
        Entrypoint = ["${dumb-init}/bin/dumb-init" "--" "${nginz}/usr/bin/nginz_reload.sh" "-g" "daemon off;" "-c" "/etc/wire/nginz/conf/nginx.conf"];
        Env = ["SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"];
        ExposedPorts = {
          "80/tcp" = {};
        };
      };
    };
in
  nginzImage
