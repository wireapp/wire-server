{ mkDerivation, async, attoparsec, base, bytestring
, bytestring-builder, conduit, directory, exceptions, filepath
, gauge, hspec, lib, network, primitive, process, QuickCheck
, resourcet, stm, streaming-commons, text, transformers
, transformers-base, typed-process, unliftio-core
}:
mkDerivation {
  pname = "conduit-extra";
  version = "1.3.5";
  sha256 = "8a648dee203c01e647fa386bfe7a5b293ce552f8b5cab9c0dd5cb71c7cd012d9";
  libraryHaskellDepends = [
    async attoparsec base bytestring conduit directory filepath network
    primitive process resourcet stm streaming-commons text transformers
    typed-process unliftio-core
  ];
  testHaskellDepends = [
    async attoparsec base bytestring bytestring-builder conduit
    directory exceptions filepath hspec process QuickCheck resourcet
    stm streaming-commons text transformers transformers-base
  ];
  benchmarkHaskellDepends = [
    base bytestring bytestring-builder conduit gauge transformers
  ];
  homepage = "http://github.com/snoyberg/conduit";
  description = "Batteries included conduit: adapters for common libraries";
  license = lib.licenses.mit;
}
