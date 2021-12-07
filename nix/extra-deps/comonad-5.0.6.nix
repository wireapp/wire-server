{ mkDerivation, base, Cabal, cabal-doctest, containers
, distributive, doctest, lib, tagged, transformers
, transformers-compat
}:
mkDerivation {
  pname = "comonad";
  version = "5.0.6";
  sha256 = "77cfb016acd1747b892c31b82daf0de93f508ba775d62562d376b354adb88fae";
  revision = "1";
  editedCabalFile = "19744zfb5nd90a3xnhl7fx9aik39nwwx9sf7k9aahrcplwlvbwgx";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base containers distributive tagged transformers
    transformers-compat
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "http://github.com/ekmett/comonad/";
  description = "Comonads";
  license = lib.licenses.bsd3;
}
