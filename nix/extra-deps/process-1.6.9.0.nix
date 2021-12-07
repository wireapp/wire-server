{ mkDerivation, base, bytestring, deepseq, directory, filepath, lib
, unix
}:
mkDerivation {
  pname = "process";
  version = "1.6.9.0";
  sha256 = "fefbd313ca68df84e6159e25ce2311b4463f3c2049bcd69dc38d6b9106e51dea";
  revision = "2";
  editedCabalFile = "17m1xsxbg7fmmp0x7yj5y50xdf69cvs5v79609nhxm59zsbffb9p";
  libraryHaskellDepends = [ base deepseq directory filepath unix ];
  testHaskellDepends = [ base bytestring directory ];
  description = "Process libraries";
  license = lib.licenses.bsd3;
}
