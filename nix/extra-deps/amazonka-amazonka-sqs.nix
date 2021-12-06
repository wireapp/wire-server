{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, fetchgit, lib, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sqs";
  version = "1.6.1";
  src = fetchgit {
    url = "https://github.com/wireapp/amazonka";
    sha256 = "10ckgr1g3ky37hcn34n68ma878mkpdzb011nn84fzfm6h7bp70hn";
    rev = "412172d8c28906591f01576a78792de7c34cc3eb";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/amazonka-sqs; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Queue Service SDK";
  license = lib.licenses.mpl20;
}
