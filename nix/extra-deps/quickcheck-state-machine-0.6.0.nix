{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, directory, doctest, exceptions, filelock, filepath, http-client
, lib, matrix, monad-logger, mtl, network, persistent
, persistent-postgresql, persistent-template, pretty-show, process
, QuickCheck, quickcheck-instances, random, resourcet, servant
, servant-client, servant-server, strict, string-conversions, tasty
, tasty-hunit, tasty-quickcheck, text, tree-diff, unliftio, vector
, wai, warp
}:
mkDerivation {
  pname = "quickcheck-state-machine";
  version = "0.6.0";
  sha256 = "3e5f7199282c185986eedbf7cd22e2c68d4ec6ef24bec80c27a33429c555727d";
  libraryHaskellDepends = [
    ansi-wl-pprint base containers exceptions matrix mtl pretty-show
    QuickCheck tree-diff unliftio vector
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest filelock filepath
    http-client matrix monad-logger mtl network persistent
    persistent-postgresql persistent-template pretty-show process
    QuickCheck quickcheck-instances random resourcet servant
    servant-client servant-server strict string-conversions tasty
    tasty-hunit tasty-quickcheck text tree-diff unliftio vector wai
    warp
  ];
  homepage = "https://github.com/advancedtelematic/quickcheck-state-machine#readme";
  description = "Test monadic programs using state machine based models";
  license = lib.licenses.bsd3;
}
