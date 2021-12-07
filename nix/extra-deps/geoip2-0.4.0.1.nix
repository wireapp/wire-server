{ mkDerivation, base, bytestring, cereal, containers, iproute, lens
, lib, mmap, reinterpret-cast, text
}:
mkDerivation {
  pname = "geoip2";
  version = "0.4.0.1";
  sha256 = "275e773a830a5826b6edf1a0ec7c49b4b8341ac0b037933bbdeeea83aba30c60";
  libraryHaskellDepends = [
    base bytestring cereal containers iproute lens mmap
    reinterpret-cast text
  ];
  description = "Pure haskell interface to MaxMind GeoIP database";
  license = lib.licenses.bsd3;
}
