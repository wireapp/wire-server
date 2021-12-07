{ mkDerivation, base, bytestring, cereal, conduit, HUnit, lib, mtl
, resourcet, transformers
}:
mkDerivation {
  pname = "cereal-conduit";
  version = "0.8.0";
  sha256 = "d95c4518a9984feacfd811c64be993705bff74c1f2daa00b4687bbb79f3a39eb";
  revision = "1";
  editedCabalFile = "1imyl3g2bni8bc6kajr857xh94fscphksj3286pxfpa8yp9vqqpc";
  libraryHaskellDepends = [
    base bytestring cereal conduit resourcet transformers
  ];
  testHaskellDepends = [
    base bytestring cereal conduit HUnit mtl transformers
  ];
  homepage = "https://github.com/snoyberg/conduit";
  description = "Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits";
  license = lib.licenses.bsd3;
}
