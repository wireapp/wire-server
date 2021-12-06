{ mkDerivation, amazonka-core, base, bytestring, conduit
, conduit-extra, directory, exceptions, fetchgit, http-client
, http-conduit, http-types, ini, lib, mmorph, monad-control, mtl
, primitive, resourcet, retry, tasty, tasty-hunit, text, time
, transformers, transformers-base, transformers-compat
, unliftio-core, void
}:
mkDerivation {
  pname = "amazonka";
  version = "1.6.1";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "10ckgr1g3ky37hcn34n68ma878mkpdzb011nn84fzfm6h7bp70hn";
    rev = "412172d8c28906591f01576a78792de7c34cc3eb";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    amazonka-core base bytestring conduit conduit-extra directory
    exceptions http-client http-conduit http-types ini mmorph
    monad-control mtl primitive resourcet retry text time transformers
    transformers-base transformers-compat unliftio-core void
  ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = lib.licenses.mpl20;
}
