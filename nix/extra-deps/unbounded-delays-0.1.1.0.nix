{ mkDerivation, base, lib }:
mkDerivation {
  pname = "unbounded-delays";
  version = "0.1.1.0";
  sha256 = "8aa7f7d10a8d0073518804db76c3ef4c313359994ef175122341b0bce07329c7";
  revision = "1";
  editedCabalFile = "1ac0mncnk2x87qr53lpjzkjvqynbk76fnvj4g27d3da6x4k982w9";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/basvandijk/unbounded-delays";
  description = "Unbounded thread delays and timeouts";
  license = lib.licenses.bsd3;
}
