{ mkDerivation, base, bytestring, conduit, lib, resourcet }:
mkDerivation {
  pname = "libyaml";
  version = "0.1.2";
  sha256 = "8f42d66f199fcaee255326f8f770d88b0670df56b5eb78002d6058f3a45e97b5";
  libraryHaskellDepends = [ base bytestring conduit resourcet ];
  homepage = "https://github.com/snoyberg/yaml#readme";
  description = "Low-level, streaming YAML interface";
  license = lib.licenses.bsd3;
}
