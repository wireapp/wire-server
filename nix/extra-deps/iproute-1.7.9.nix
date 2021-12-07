{ mkDerivation, appar, base, byteorder, bytestring, containers
, doctest, hspec, lib, network, QuickCheck, safe
}:
mkDerivation {
  pname = "iproute";
  version = "1.7.9";
  sha256 = "5547fa599c46b854c98625d7d3b745557193704096d3a947c1d8c534a23360d4";
  revision = "2";
  editedCabalFile = "0fycdf2di8ix92k88fj1ibp6wxfwwh4pbwr8bxh1ajqrbhgj3y7w";
  libraryHaskellDepends = [
    appar base byteorder bytestring containers network
  ];
  testHaskellDepends = [
    appar base byteorder bytestring containers doctest hspec network
    QuickCheck safe
  ];
  homepage = "http://www.mew.org/~kazu/proj/iproute/";
  description = "IP Routing Table";
  license = lib.licenses.bsd3;
}
