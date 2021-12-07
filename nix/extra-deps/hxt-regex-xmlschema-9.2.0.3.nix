{ mkDerivation, base, bytestring, criterion, deepseq, HUnit
, hxt-charproperties, lib, parsec, text
}:
mkDerivation {
  pname = "hxt-regex-xmlschema";
  version = "9.2.0.3";
  sha256 = "f4743ba65498d6001cdfcf5cbc3317d4bc43941be5c7030b60beb83408c892b0";
  revision = "1";
  editedCabalFile = "0vg9vnfjmq1ma6zwwv4yvfih6wbslksvsvy9b41i4f81l0hh7ia8";
  libraryHaskellDepends = [
    base bytestring hxt-charproperties parsec text
  ];
  testHaskellDepends = [ base bytestring HUnit parsec text ];
  benchmarkHaskellDepends = [
    base bytestring criterion deepseq parsec text
  ];
  homepage = "http://www.haskell.org/haskellwiki/Regular_expressions_for_XML_Schema";
  description = "A regular expression library for W3C XML Schema regular expressions";
  license = lib.licenses.mit;
}
