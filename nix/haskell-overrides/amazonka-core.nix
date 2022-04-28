{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, conduit, conduit-extra, cryptonite
, data-ordlist, deepseq, fetchgit, hashable, http-client
, http-conduit, http-types, lens, lib, memory, QuickCheck
, quickcheck-unicode, regex-posix, resourcet, scientific, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, time
, transformers, unordered-containers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "amazonka-core";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "0md658m32zrvzc8nljn58r8iw4rqxpihgdnqrhl8vnmkq6i9np51";
    rev = "7ced54b0396296307b9871d293cc0ac161e5743d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive conduit
    conduit-extra cryptonite deepseq hashable http-client http-conduit
    http-types lens memory regex-posix resourcet scientific text time
    transformers unordered-containers xml-conduit xml-types
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive conduit data-ordlist
    http-conduit http-types lens QuickCheck quickcheck-unicode tasty
    tasty-hunit tasty-quickcheck template-haskell text time
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Core data types and functionality for Amazonka libraries";
  license = lib.licenses.mpl20;
}
