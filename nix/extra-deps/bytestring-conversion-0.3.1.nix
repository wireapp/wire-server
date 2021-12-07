{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, criterion, double-conversion, lib, QuickCheck, tasty
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "bytestring-conversion";
  version = "0.3.1";
  sha256 = "13b7ea48737dc7a7fd4c894ff1fb9344cf8d9ef8f4201e813d578b613e874ef8";
  revision = "2";
  editedCabalFile = "1x8c42cfzb6fdvgkxxdxcpdf16csimlzsgahb1axnplmr6b3ba63";
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive double-conversion text
  ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion text transformers
  ];
  homepage = "https://github.com/twittner/bytestring-conversion/";
  description = "Type-classes to convert values to and from ByteString";
  license = lib.licenses.mpl20;
}
