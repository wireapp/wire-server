{ stdenv
, dockerTools
, gnugrep
, coreutils
, which
, dumb-init
, bashInteractive
, lib
, makeWrapper
, writers
, dig
, gawk
, diffutils
}:
let
  nginz-disco = stdenv.mkDerivation {
    name = "nginz-disco";
    src = (writers.writeBash "nginz_disco.sh" ../tools/nginz_disco/nginz_disco.sh);
    phases = "installPhase";
    nativeBuildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/nginz_disco.sh
      wrapProgram $out/bin/nginz_disco.sh \
        --prefix PATH : "${lib.makeBinPath [ gnugrep gawk dig diffutils ]}"
    '';
  };

  nginz-disco-image = dockerTools.streamLayeredImage {
    name = "quay.io/wire/nginz_disco";
    maxLayers = 10;
    contents = [
      bashInteractive
      coreutils
      dockerTools.usrBinEnv
      dockerTools.fakeNss
      which
    ];
    config = {
      Entrypoint = [ "${dumb-init}/bin/dumb-init" "--" "${nginz-disco}/bin/nginz_disco.sh" ];
      User = "65534";
    };
  };
in
nginz-disco-image
