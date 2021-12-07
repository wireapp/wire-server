{ mkDerivation, base, comonad, containers, distributive, exceptions
, lib, mtl, profunctors, semigroupoids, template-haskell
, transformers, transformers-base
}:
mkDerivation {
  pname = "free";
  version = "5.1.3";
  sha256 = "2c70d66e3a1ad52ce4b22d5510ffc6d7b3db950bd7f43bc61801cfe7b24c2e2d";
  revision = "1";
  editedCabalFile = "0w0bxkdsspw1blhr01bb9n98wlmqpgjpdbbp2f50qmhapav2fk30";
  libraryHaskellDepends = [
    base comonad containers distributive exceptions mtl profunctors
    semigroupoids template-haskell transformers transformers-base
  ];
  homepage = "http://github.com/ekmett/free/";
  description = "Monads for free";
  license = lib.licenses.bsd3;
}
