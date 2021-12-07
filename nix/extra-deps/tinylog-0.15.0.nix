{ mkDerivation, base, bytestring, containers, criterion
, double-conversion, fast-logger, lib, text, transformers
, unix-time
}:
mkDerivation {
  pname = "tinylog";
  version = "0.15.0";
  sha256 = "cb71164ce47af991babc9837e3e86c4f9d4e0cac1440f56faa71f4497af58d5f";
  libraryHaskellDepends = [
    base bytestring containers double-conversion fast-logger text
    transformers unix-time
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://gitlab.com/twittner/tinylog/";
  description = "Simplistic logging using fast-logger";
  license = lib.licenses.mpl20;
}
