{ mkDerivation, atomic-primops, base, bytestring, containers
, http-client, http-client-tls, http-types, lib, network-uri, text
, transformers, wai, warp
}:
mkDerivation {
  pname = "prometheus";
  version = "2.2.2";
  sha256 = "4ec351ed21d2c8f6f3132640a8aeaa7fc62d9f9d0f6ef76861ef3bb2e7642387";
  libraryHaskellDepends = [
    atomic-primops base bytestring containers http-client
    http-client-tls http-types network-uri text transformers wai warp
  ];
  homepage = "http://github.com/bitnomial/prometheus";
  description = "Prometheus Haskell Client";
  license = lib.licenses.bsd3;
}
