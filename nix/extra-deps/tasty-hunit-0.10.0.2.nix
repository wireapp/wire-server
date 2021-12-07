{ mkDerivation, base, call-stack, lib, tasty }:
mkDerivation {
  pname = "tasty-hunit";
  version = "0.10.0.2";
  sha256 = "4823c85efe15f36e1d71867aaa6cdcdff3ef39f97492321e9bb8a30c742f6ef7";
  libraryHaskellDepends = [ base call-stack tasty ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "HUnit support for the Tasty test framework";
  license = lib.licenses.mit;
}
