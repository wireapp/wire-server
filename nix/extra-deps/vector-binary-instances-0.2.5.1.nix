{ mkDerivation, base, binary, bytestring, deepseq, gauge, lib
, tasty, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "vector-binary-instances";
  version = "0.2.5.1";
  sha256 = "3945b99f8efd319c837700b7cef5163ee23e656e89227357e0b7a41d2a66c512";
  revision = "2";
  editedCabalFile = "0ia9i7q7jrk3ab3nq2368glr69vl6fwvh42zlwvdmxn4xd861qfx";
  libraryHaskellDepends = [ base binary vector ];
  testHaskellDepends = [ base binary tasty tasty-quickcheck vector ];
  benchmarkHaskellDepends = [
    base binary bytestring deepseq gauge vector
  ];
  homepage = "https://github.com/bos/vector-binary-instances";
  description = "Instances of Data.Binary for vector";
  license = lib.licenses.bsd3;
}
