{ mkDerivation, base, containers, hashable, lib, syb
, unordered-containers
}:
mkDerivation {
  pname = "uniplate";
  version = "1.6.12";
  sha256 = "fcc60bc6b3f6e925f611646db90e6db9f05286a9363405f844df1dc15572a8b7";
  revision = "1";
  editedCabalFile = "0gsrs2mk58jg3x36dyzxi4y46isd5p6q0rd6m9l834h5r7ds6a54";
  libraryHaskellDepends = [
    base containers hashable syb unordered-containers
  ];
  homepage = "http://community.haskell.org/~ndm/uniplate/";
  description = "Help writing simple, concise and fast generic operations";
  license = lib.licenses.bsd3;
}
