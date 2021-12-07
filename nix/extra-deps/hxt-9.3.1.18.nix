{ mkDerivation, base, binary, bytestring, containers, deepseq
, directory, filepath, hxt-charproperties, hxt-regex-xmlschema
, hxt-unicode, lib, mtl, network-uri, parsec
}:
mkDerivation {
  pname = "hxt";
  version = "9.3.1.18";
  sha256 = "721809d89bbcaf29b2bbe3b9cdbb54e6d6a30afe7509186061898f7e8b996620";
  libraryHaskellDepends = [
    base binary bytestring containers deepseq directory filepath
    hxt-charproperties hxt-regex-xmlschema hxt-unicode mtl network-uri
    parsec
  ];
  homepage = "https://github.com/UweSchmidt/hxt";
  description = "A collection of tools for processing XML with Haskell";
  license = lib.licenses.mit;
}
