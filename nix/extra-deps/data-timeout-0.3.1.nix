{ mkDerivation, base, data-textual, lib, parsers, stm, tagged
, text-printer, transformers-base
}:
mkDerivation {
  pname = "data-timeout";
  version = "0.3.1";
  sha256 = "66470adeb25b07c8b4996fc4f80f15cca2ab27e9a96141c5834bb494e6b4dede";
  libraryHaskellDepends = [
    base data-textual parsers stm tagged text-printer transformers-base
  ];
  homepage = "https://github.com/mvv/data-timeout";
  description = "64-bit timeouts of nanosecond precision";
  license = lib.licenses.bsd3;
}
