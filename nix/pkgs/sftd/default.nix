{ fetchFromGitHub
, stdenv
, lib
, autoconf
, automake
, gnumake
, clang
, clang-analyzer
, libtool
, pkg-config
, protobufc
, which
, rsync
, perl
, openssl
}:

let
  version = "unstable-2022-06-15";
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "wire-avs-service";
    # https://github.com/wireapp/wire-avs-service/pull/4
    rev = "ef9890b425c4b54fa4adba7a4f25336b2e390ef9";
    sha256 = "sha256-oN2ID8H9sLY+CP44nPtvPacNaxxApaSan551f/dYe4I=";
    fetchSubmodules = true;
  };

in
stdenv.mkDerivation {
  pname = "sftd";
  inherit version src;

  nativeBuildInputs = [
    autoconf
    automake
    libtool
    which
    # clang
    gnumake
    pkg-config
    clang-analyzer #scan-build

    rsync
    perl
  ];

  buildInputs = [
    protobufc

    openssl
    # TODO: have avs pick openssl from pkg-config?
  ];

  # less build log litter for now
  enableParallelBuilding = false;

  makeFlags = [
    "CXX:=$(CXX)"
    "CC:=$(CC)"
    "LD:=$(LD)"
    "RANLIB:=$(RANLIB)"
  ];
}
