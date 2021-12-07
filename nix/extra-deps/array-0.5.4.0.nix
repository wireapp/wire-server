{ mkDerivation, base, lib }:
mkDerivation {
  pname = "array";
  version = "0.5.4.0";
  sha256 = "3b135753f9697f9cafbda7ebbd98fd56cc136e7496c371d090829ad13ab7b8c7";
  libraryHaskellDepends = [ base ];
  description = "Mutable and immutable arrays";
  license = lib.licenses.bsd3;
}
