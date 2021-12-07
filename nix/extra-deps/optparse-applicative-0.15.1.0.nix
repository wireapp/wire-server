{ mkDerivation, ansi-wl-pprint, base, bytestring, lib, process
, QuickCheck, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.15.1.0";
  sha256 = "4db3675fd1e0594afdf079db46f4cd412d483835d703e7c07e1a1a37d6f046f3";
  revision = "1";
  editedCabalFile = "0zmhqkd96v2z1ilhqdkd9z4jgsnsxb8yi2479ind8m5zm9363zr9";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base bytestring QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = lib.licenses.bsd3;
}
