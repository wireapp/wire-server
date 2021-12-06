{ mkDerivation, attoparsec, base, base58-bytestring
, base64-bytestring, byteable, bytestring, cryptohash, fetchgit
, hex, io-streams, lib, optparse-applicative
}:
mkDerivation {
  pname = "multihash";
  version = "0.1.6";
  src = fetchgit {
    url = "https://github.com/wireapp/haskell-multihash.git";
    sha256 = "0lcm6argp49fin4va7c50l1lj84xcm3cqzijzssfdgplimpmllma";
    rev = "300a6f46384bfca33e545c8bab52ef3717452d12";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base58-bytestring base64-bytestring bytestring
    cryptohash hex io-streams
  ];
  executableHaskellDepends = [
    base base58-bytestring base64-bytestring byteable bytestring
    cryptohash hex io-streams optparse-applicative
  ];
  homepage = "https://github.com/LukeHoersten/multihash";
  description = "Multihash library and CLI executable";
  license = lib.licenses.bsd3;
}
