{ mkDerivation, attoparsec, base, blaze-html, blaze-markup
, bytestring, conduit, conduit-extra, containers
, data-default-class, deepseq, doctest, hspec, HUnit, lib
, resourcet, text, transformers, xml-types
}:
mkDerivation {
  pname = "xml-conduit";
  version = "1.9.0.0";
  sha256 = "1cb4b8c3571dc044d0001e35f2c3e3a28f591bb6bb4a5ef7ee59207444d8a7dc";
  libraryHaskellDepends = [
    attoparsec base blaze-html blaze-markup bytestring conduit
    conduit-extra containers data-default-class deepseq resourcet text
    transformers xml-types
  ];
  testHaskellDepends = [
    base blaze-markup bytestring conduit containers doctest hspec HUnit
    resourcet text transformers xml-types
  ];
  homepage = "http://github.com/snoyberg/xml";
  description = "Pure-Haskell utilities for dealing with XML with the conduit package";
  license = lib.licenses.mit;
}
