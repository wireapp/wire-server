{ mkDerivation, base, base-orphans, bifunctors, comonad
, contravariant, distributive, lib, tagged, transformers
}:
mkDerivation {
  pname = "profunctors";
  version = "5.5.2";
  sha256 = "5f1579aab8afae377c7c7c0d0ed95b0bc58003b6dad2d494045f7472a398af7c";
  revision = "1";
  editedCabalFile = "0b73nqyl3yc3q4r0asrciqx7q19y6ygfszb79kn37pndnzj4m0hd";
  libraryHaskellDepends = [
    base base-orphans bifunctors comonad contravariant distributive
    tagged transformers
  ];
  homepage = "http://github.com/ekmett/profunctors/";
  description = "Profunctors";
  license = lib.licenses.bsd3;
}
