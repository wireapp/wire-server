{ mkDerivation, base, lib, old-locale, time }:
mkDerivation {
  pname = "time-locale-compat";
  version = "0.1.1.5";
  sha256 = "07ff1566de7d851423a843b2de385442319348c621d4f779b3d365ce91ac502c";
  libraryHaskellDepends = [ base old-locale time ];
  homepage = "https://github.com/khibino/haskell-time-locale-compat";
  description = "Compatibile module for time-format locale";
  license = lib.licenses.bsd3;
}
