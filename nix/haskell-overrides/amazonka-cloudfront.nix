{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lib, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-cloudfront";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "0md658m32zrvzc8nljn58r8iw4rqxpihgdnqrhl8vnmkq6i9np51";
    rev = "7ced54b0396296307b9871d293cc0ac161e5743d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-cloudfront; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon CloudFront SDK";
  license = lib.licenses.mpl20;
}
