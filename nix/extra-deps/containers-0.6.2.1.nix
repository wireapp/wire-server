{ mkDerivation, array, base, deepseq, lib }:
mkDerivation {
  pname = "containers";
  version = "0.6.2.1";
  sha256 = "130c5794f35f322e0d995c413e8e0b262e1d3cb617ecbf21a892f80e0ed30016";
  revision = "1";
  editedCabalFile = "1q9yykjcpwxq4j2ps6bhj8mhz7mim3wg0q8yl8199hvhdwl3j2sm";
  libraryHaskellDepends = [ array base deepseq ];
  description = "Assorted concrete container types";
  license = lib.licenses.bsd3;
}
