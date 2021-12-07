{ mkDerivation, attoparsec, base, bytestring, directory, filepath
, hashable, HUnit, lib, test-framework, test-framework-hunit, text
, unix-compat, unordered-containers
}:
mkDerivation {
  pname = "configurator";
  version = "0.3.0.0";
  sha256 = "6eb9996b672e9f7112ca23482c42fa533553312c3c13f38a8a06476e67c031b4";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base bytestring directory hashable text unix-compat
    unordered-containers
  ];
  testHaskellDepends = [
    base bytestring directory filepath HUnit test-framework
    test-framework-hunit text
  ];
  homepage = "http://github.com/bos/configurator";
  description = "Configuration management";
  license = lib.licenses.bsd3;
}
