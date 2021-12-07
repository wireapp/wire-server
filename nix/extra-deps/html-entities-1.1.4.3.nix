{ mkDerivation, attoparsec, base, base-prelude, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "html-entities";
  version = "1.1.4.3";
  sha256 = "ceeab562184a5921381e41eddd7a91dc98e85431ab445e286320ba3aa0ad5556";
  libraryHaskellDepends = [
    attoparsec base base-prelude text unordered-containers
  ];
  homepage = "https://github.com/nikita-volkov/html-entities";
  description = "A codec library for HTML-escaped text and HTML-entities";
  license = lib.licenses.mit;
}
