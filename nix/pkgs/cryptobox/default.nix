{ fetchFromGitHub
, lib
, libsodium
, pkg-config
, naersk
, stdenv
}:

naersk.buildPackage rec {
  name = "cryptobox-c-${version}";
  version = "2019-10-22";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "cryptobox-c";
    rev = "10826e3c54b0e8d854d8d80acb9387facd36bb61";
    sha256 = "sha256-Tg2yLKkRIor5hep1fWv4smnobhwF8+z7WfR0bv713D0=";
  };

  postInstall = ''
    mkdir -p $out/include
    cp src/cbox.h $out/include
  '';
}
