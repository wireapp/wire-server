{ mkDerivation, array, base, binary, bytestring, fetchgit, lib
, snappy
}:
mkDerivation {
  pname = "snappy-framing";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/kim/snappy-framing";
    sha256 = "04z8qw5jaw58c09bf29w5k8hp7xa5w69c14ad672crw8zgsw7860";
    rev = "d99f702c0086729efd6848dea8a01e5266c3a61c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base binary bytestring snappy ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/kim/snappy-framing";
  description = "Snappy Framing Format in Haskell";
  license = lib.licenses.mpl20;
}
