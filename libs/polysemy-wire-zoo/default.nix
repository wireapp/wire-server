{ mkDerivation, base, HsOpenSSL, hspec, imports, lib, polysemy
, polysemy-check, polysemy-plugin, QuickCheck, saml2-web-sso, time
, tinylog, types-common, uuid
}:
mkDerivation {
  pname = "polysemy-wire-zoo";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base HsOpenSSL hspec imports polysemy polysemy-check
    polysemy-plugin QuickCheck saml2-web-sso time tinylog types-common
    uuid
  ];
  description = "Polysemy interface for various libraries";
  license = lib.licenses.agpl3Only;
}
