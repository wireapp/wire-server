{ mkDerivation, alex, array, base, binary, bytestring, containers
, deepseq, directory, filepath, ghc-prim, happy, hpc, lib, pretty
, process, time, transformers, unix
}:
mkDerivation {
  pname = "ghc-lib-parser";
  version = "8.10.1.20200412";
  sha256 = "3fe0f0239f133427222d530109f81c05ceb9aa32e10c8489dbe5bb491c75cd32";
  revision = "1";
  editedCabalFile = "1b63krgjr6m82i2prclivvq5nlvj1b91mapy045ipmbv8nxpwp9l";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    ghc-prim hpc pretty process time transformers unix
  ];
  libraryToolDepends = [ alex happy ];
  homepage = "https://github.com/digital-asset/ghc-lib";
  description = "The GHC API, decoupled from GHC versions";
  license = lib.licenses.bsd3;
}
