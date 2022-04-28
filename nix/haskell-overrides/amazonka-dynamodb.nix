{ mkDerivation, aeson, amazonka-core, amazonka-test, base
, bytestring, case-insensitive, containers, fetchgit, hashable, lib
, tasty, tasty-hunit, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "amazonka-dynamodb";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "0md658m32zrvzc8nljn58r8iw4rqxpihgdnqrhl8vnmkq6i9np51";
    rev = "7ced54b0396296307b9871d293cc0ac161e5743d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-dynamodb; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core base containers hashable unordered-containers
    vector
  ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon DynamoDB SDK";
  license = lib.licenses.mpl20;
}
