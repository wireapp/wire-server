{ mkDerivation, attoparsec, base, bytestring, lib, mime, split
, text, utf8-string, word8
}:
mkDerivation {
  pname = "stompl";
  version = "0.5.0";
  sha256 = "b0538c190c3fa1f63d81aa2518561c2ae6dd1407f86b56794a2024e9b59a5158";
  libraryHaskellDepends = [
    attoparsec base bytestring mime split text utf8-string word8
  ];
  homepage = "http://github.com/toschoo/mom";
  description = "Stomp Parser and Utilities";
  license = "LGPL";
}
