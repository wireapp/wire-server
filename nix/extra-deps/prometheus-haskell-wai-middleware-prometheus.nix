{ mkDerivation, base, bytestring, clock, data-default, doctest
, fetchgit, http-types, lib, prometheus-client, text, wai
}:
mkDerivation {
  pname = "wai-middleware-prometheus";
  version = "1.0.0.1";
  src = fetchgit {
    url = "https://github.com/fimad/prometheus-haskell";
    sha256 = "1xg3jyhy60xxhcwcl8sc55r7yzya0nqjl8bchms6cvfnzldrcih5";
    rev = "43f19dae23f1e374c6e99eed6840ce185cca66c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wai-middleware-prometheus; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring clock data-default http-types prometheus-client
    text wai
  ];
  testHaskellDepends = [ base doctest prometheus-client ];
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "WAI middlware for exposing http://prometheus.io metrics.";
  license = lib.licenses.asl20;
}
