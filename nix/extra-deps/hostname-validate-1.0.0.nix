{ mkDerivation, attoparsec, base, bytestring, lib }:
mkDerivation {
  pname = "hostname-validate";
  version = "1.0.0";
  sha256 = "7fafb1e0cfe19d453030754962e74cdb8f3e791ec5b974623cbf26872779c857";
  libraryHaskellDepends = [ attoparsec base bytestring ];
  description = "Validate hostnames e.g. localhost or foo.co.uk.";
  license = lib.licenses.bsd3;
}
