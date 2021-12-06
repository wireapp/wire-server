{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, errors, exceptions, fetchgit, hashable, hpack, hspec, http-client
, http-types, lib, microlens, microlens-aeson, mtl, network-uri
, pretty-simple, QuickCheck, quickcheck-arbitrary-template
, quickcheck-properties, scientific, semigroups, semver, temporary
, text, time, transformers, unix-compat, unordered-containers
, vector
}:
mkDerivation {
  pname = "bloodhound";
  version = "0.17.0.0";
  src = fetchgit {
    url = "https://github.com/wireapp/bloodhound";
    sha256 = "07mb9dv7lfhm5pxkj6i80kk5ypbig03vkzqvyw88za7a5i7h63zb";
    rev = "fc310b23c3c1698ff7a5ca3c54236ef84846dbf3";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers exceptions hashable
    http-client http-types mtl network-uri scientific semigroups semver
    text time transformers unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base blaze-builder bytestring containers errors exceptions
    hashable hspec http-client http-types microlens microlens-aeson mtl
    network-uri pretty-simple QuickCheck quickcheck-arbitrary-template
    quickcheck-properties scientific semigroups semver temporary text
    time transformers unix-compat unordered-containers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/bitemyapp/bloodhound.git#readme";
  description = "Elasticsearch client library for Haskell";
  license = lib.licenses.bsd3;
}
