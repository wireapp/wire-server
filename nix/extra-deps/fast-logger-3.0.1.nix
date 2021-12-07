{ mkDerivation, array, auto-update, base, bytestring, directory
, easy-file, filepath, hspec, hspec-discover, lib, text
, unix-compat, unix-time
}:
mkDerivation {
  pname = "fast-logger";
  version = "3.0.1";
  sha256 = "45080ca54a3862ed22f78c2bcb56d023695c4f4fd37dbbac418248c07cdca53b";
  revision = "1";
  editedCabalFile = "0i5cjwrvdvnx3qi03crkyrkfb4ry61imngwcwr8lpaxbpmz0sjsj";
  libraryHaskellDepends = [
    array auto-update base bytestring directory easy-file filepath text
    unix-compat unix-time
  ];
  testHaskellDepends = [ base bytestring directory hspec ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/kazu-yamamoto/logger";
  description = "A fast logging system";
  license = lib.licenses.bsd3;
}
