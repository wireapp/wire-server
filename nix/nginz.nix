{ nginxModules
, pkg-config
, callPackage
, stdenvNoCC
, dockerTools
, nginx
, inotify-tools
, dumb-init
, cacert
, bashInteractive
, openssl
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
      '';
    };

    # Use the following to get the digest and sha:
    # nix-shell -p pkgs.nix-prefetch-docker --run 'nix-prefetch-docker --image-name alpine --image-tag 3.15'
    # And the sha this gives back seems incorrect, so build it, wait for the error, then adjust the sha again.
    alpine-image = dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:69463fdff1f025c908939e86d4714b4d5518776954ca627cbeff4c74bcea5b22";
      sha256 = "sha256-LTayEaBtNOho1mhGyivKLZdu5+pSL3SZWbIAMpUJM4E=";
    };

    nginzImage = dockerTools.buildLayeredImage {
      name = "quay.io/wire/nginz";
      maxLayers = 5;
      fromImage = alpine-image;
      contents = [
        cacert
        bashInteractive
        openssl
        dumb-init
        inotify-tools
        reload-script
        # nginz # adding this removes the 'apk' command from alpine for some reason...
      ];
      # TODO add user and group for nginx, doesn't work! :(
      enableFakechroot = true;
      fakeRootCommands = ''
          /sbin/apk update
          /sbin/apk add curl
          # nginx still tries to read this directory even if error_log
          # directive is specifying another file :/
          mkdir -p /var/log/nginx
          mkdir -p /var/cache/nginx
          # nginz config hardcodes nginx user/group
          addgroup -g 101 -S nginx
          adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx
      '';
      config = {
        Entrypoint = ["${dumb-init}/bin/dumb-init" "--" "/usr/bin/nginz_reload.sh" "-g" "daemon off;" "-c" "/etc/wire/nginz/conf/nginx.conf"];
        Env = ["SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"];
        ExposedPorts = {
          "80/tcp" = {};
        };
      };
    };
in
  nginzImage
