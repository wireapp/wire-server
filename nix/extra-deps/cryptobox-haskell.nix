{ mkDerivation, base, bytestring, cryptobox, fetchgit, hashable
, lib, unordered-containers
}:
mkDerivation {
  pname = "cryptobox-haskell";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/wireapp/cryptobox-haskell";
    sha256 = "0dgizj1kc135yzzqdf5l7f5ax0qpvrr8mxvg7s1dbm01cf11aqzn";
    rev = "7546a1a25635ef65183e3d44c1052285e8401608";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring hashable unordered-containers
  ];
  librarySystemDepends = [ cryptobox ];
  homepage = "https://github.com/wireapp/cryptobox-haskell/";
  description = "Haskell bindings to cryptobox";
  license = lib.licenses.gpl3Only;
}
