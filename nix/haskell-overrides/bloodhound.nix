{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, deriving-aeson, errors, exceptions, fetchgit, hashable, hspec
, http-client, http-types, lib, microlens, microlens-aeson, mtl
, network-uri, pretty-simple, QuickCheck
, quickcheck-arbitrary-template, quickcheck-properties, scientific
, semigroups, semver, temporary, text, time, transformers
, unix-compat, unordered-containers, vector
}:
mkDerivation {
  pname = "bloodhound";
  version = "0.17.0.0";
  src = fetchgit {
    url = "https://github.com/wireapp/bloodhound";
    sha256 = "0fr5xgq8f1nmcbk8srrhyf4vad4xm5iqr974jgqfg1mg31y85h0x";
    rev = "c68e6d877d87988331bf391ed16780383a58eefa";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers deriving-aeson
    exceptions hashable http-client http-types mtl network-uri
    scientific semigroups semver text time transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base blaze-builder bytestring containers deriving-aeson
    errors exceptions hashable hspec http-client http-types microlens
    microlens-aeson mtl network-uri pretty-simple QuickCheck
    quickcheck-arbitrary-template quickcheck-properties scientific
    semigroups semver temporary text time transformers unix-compat
    unordered-containers vector
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/bitemyapp/bloodhound.git#readme";
  description = "Elasticsearch client library for Haskell";
  license = lib.licenses.bsd3;
}
