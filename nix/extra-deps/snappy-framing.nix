{ mkDerivation, array, base, binary, bytestring, fetchgit, lib
, snappy
}:
mkDerivation {
  pname = "snappy-framing";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/kim/snappy-framing";
    sha256 = "1x01bnsmks1zdrcwr7xvzlppbrwdhx4mq03787q71l5ck1bwb3d5";
    rev = "3ba2cca4182ef65b02396471b3e0c9e462e6d16c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base binary bytestring snappy ];
  homepage = "https://github.com/kim/snappy-framing";
  description = "Snappy Framing Format in Haskell";
  license = lib.licenses.mpl20;
}
