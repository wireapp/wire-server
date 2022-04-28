{ mkDerivation, aeson, amazonka-core, amazonka-sso, amazonka-sts
, base, bytestring, conduit, directory, exceptions, fetchgit
, http-client, http-conduit, http-types, ini, lens, lib, resourcet
, retry, text, time, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "amazonka";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "0md658m32zrvzc8nljn58r8iw4rqxpihgdnqrhl8vnmkq6i9np51";
    rev = "7ced54b0396296307b9871d293cc0ac161e5743d";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core amazonka-sso amazonka-sts base bytestring
    conduit directory exceptions http-client http-conduit http-types
    ini lens resourcet retry text time transformers
    unordered-containers uuid
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = lib.licenses.mpl20;
}
