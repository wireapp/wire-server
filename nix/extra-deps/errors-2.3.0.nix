{ mkDerivation, base, exceptions, lib, safe, text, transformers
, transformers-compat
}:
mkDerivation {
  pname = "errors";
  version = "2.3.0";
  sha256 = "6772e5689f07e82077ffe3339bc672934d83d83a97a7d4f1349de1302cb71f75";
  revision = "1";
  editedCabalFile = "08y607nwnw12vlbmwcppv1ydw726x8p3kwwx4rgaiss906hgnzrp";
  libraryHaskellDepends = [
    base exceptions safe text transformers transformers-compat
  ];
  description = "Simplified error-handling";
  license = lib.licenses.bsd3;
}
