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

  # Docker tools doesn't create tmp directories but nginx needs this and so we
  # have to create it ourself.
  tmpDir = runCommand "tmp-dir" { } ''
    mkdir -p $out/tmp
    mkdir -p $out/var/cache/nginx
    mkdir -p $out/var/log/nginx
    mkdir -p $out/var/run
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
      dockerTools.fakeNss
      dockerTools.usrBinEnv
      nginz # so preStop lifecycle hook in cannon can nginx -c … quit
      tmpDir
    ];
    # Any mkdir running in this step won't actually make it to the image,
    # hence we use the tmpDir derivation in the contents
    fakeRootCommands = ''
      chmod 1777 tmp
      chmod 1777 var/tmp
      chmod 1777 var/run
      chmod 1777 var/log/nginx
      chmod 1777 var/cache/nginx
    '';
    config = {
      Entrypoint = [ "${dumb-init}/bin/dumb-init" "--" "${nginzWithReloader}/bin/nginz_reload.sh" "-g" "daemon off;" "-c" "/etc/wire/nginz/conf/nginx.conf" ];
      Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" ];
      User = "65534";
    };
  };
in
nginzImage
