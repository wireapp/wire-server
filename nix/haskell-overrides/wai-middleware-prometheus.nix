{ mkDerivation, base, bytestring, clock, data-default, doctest
, fetchgit, http-types, lib, prometheus-client, text, wai
}:
mkDerivation {
  pname = "wai-middleware-prometheus";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/fimad/prometheus-haskell";
    sha256 = "0vfzysn9sgpxymfvpahxrp74fczgjnw3kgknj6zk0473qk85488f";
    rev = "2e3282e5fb27ba8d989c271a0a989823fad7ec43";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wai-middleware-prometheus; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring clock data-default http-types prometheus-client
    text wai
  ];
  testHaskellDepends = [ base doctest prometheus-client ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "WAI middlware for exposing http://prometheus.io metrics.";
  license = lib.licenses.asl20;
}
