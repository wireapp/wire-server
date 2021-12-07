{ mkDerivation, base, hxt, hxt-charproperties, invertible, lib, mtl
}:
mkDerivation {
  pname = "invertible-hxt";
  version = "0.1";
  sha256 = "add3ba846667b933d8fd4bbe20a1f68be02716e6e047eb711b96a14975a88e94";
  libraryHaskellDepends = [
    base hxt hxt-charproperties invertible mtl
  ];
  description = "invertible transformer instances for HXT Picklers";
  license = lib.licenses.bsd3;
}
