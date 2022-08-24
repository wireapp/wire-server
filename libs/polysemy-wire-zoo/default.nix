{ mkDerivation, base, cassandra-util, HsOpenSSL, hspec, imports
, lib, polysemy, polysemy-check, polysemy-plugin, QuickCheck
, saml2-web-sso, time, tinylog, types-common, uuid, wire-api
}:
mkDerivation {
  pname = "polysemy-wire-zoo";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base cassandra-util HsOpenSSL hspec imports polysemy polysemy-check
    polysemy-plugin QuickCheck saml2-web-sso time tinylog types-common
    uuid wire-api
  ];
  description = "Polysemy interface for various libraries";
  license = lib.licenses.agpl3Only;
}
