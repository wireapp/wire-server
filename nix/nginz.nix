{ stdenv
, symlinkJoin
, dockerTools
, writeTextDir
, runCommand
, gnugrep
, coreutils
, which
, inotify-tools
, dumb-init
, cacert
, bashInteractive
, lib
, makeWrapper
, writers
, nginz
}:
let

  nginzWithReloader = stdenv.mkDerivation {
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
  nginxFakeNss = symlinkJoin {
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

  # Docker tools doesn't create tmp directories but nginx needs this and so we
  # have to create it ourself.
  tmpDir = runCommand "tmp-dir" { } ''
    mkdir -p $out/tmp
    mkdir -p $out/var/tmp
  '';

  nginzImage = dockerTools.streamLayeredImage {
    name = "quay.io/wire/nginz";
    maxLayers = 10;
    contents = [
      cacert
      bashInteractive
      gnugrep
      which
      coreutils
      nginxFakeNss
      nginz # so preStop lifecycle hook in cannon can nginx -c … quit
      tmpDir
    ];
    # Any mkdir running in this step won't actually make it to the image,
    # hence we use the tmpDir derivation in the contents
    fakeRootCommands = ''
      chmod 1777 tmp
      chmod 1777 var/tmp
    '';
    config = {
      Entrypoint = [ "${dumb-init}/bin/dumb-init" "--" "${nginzWithReloader}/bin/nginz_reload.sh" "-g" "daemon off;" "-c" "/etc/wire/nginz/conf/nginx.conf" ];
      Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" ];
    };
  };
in
nginzImage
