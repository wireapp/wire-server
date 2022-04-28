{ mkDerivation, base, dns, hspec, hspec-discover, imports, iproute
, lib, polysemy, random
}:
mkDerivation {
  pname = "dns-util";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base dns imports iproute polysemy random
  ];
  testHaskellDepends = [
    base dns hspec imports iproute polysemy random
  ];
  testToolDepends = [ hspec-discover ];
  description = "Library to deal with DNS SRV records";
  license = lib.licenses.agpl3Only;
}
