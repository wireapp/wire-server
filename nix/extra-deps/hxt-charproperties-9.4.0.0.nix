{ mkDerivation, base, lib }:
mkDerivation {
  pname = "hxt-charproperties";
  version = "9.4.0.0";
  sha256 = "f50ea1d5528025400507f9b181596f7b277e645a1e33ebbc659be22b244468ae";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/UweSchmidt/hxt";
  description = "Character properties and classes for XML and Unicode";
  license = lib.licenses.mit;
}
