{ mkDerivation, base, lib }:
mkDerivation {
  pname = "old-locale";
  version = "1.0.0.7";
  sha256 = "dbaf8bf6b888fb98845705079296a23c3f40ee2f449df7312f7f7f1de18d7b50";
  revision = "2";
  editedCabalFile = "04b9vn007hlvsrx4ksd3r8r3kbyaj2kvwxchdrmd4370qzi8p6gs";
  libraryHaskellDepends = [ base ];
  description = "locale library";
  license = lib.licenses.bsd3;
}
