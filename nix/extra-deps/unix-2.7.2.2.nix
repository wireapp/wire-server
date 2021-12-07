{ mkDerivation, base, bytestring, lib, time }:
mkDerivation {
  pname = "unix";
  version = "2.7.2.2";
  sha256 = "98dd4eb1b28d65bb57f42acbe22076930c0ad5947f3c1459ab7b15abd57cdeac";
  revision = "8";
  editedCabalFile = "1ydydm9i82pn5sy7drl404qlll318x8bjwhpvr2lwqwmb5f3dx8m";
  libraryHaskellDepends = [ base bytestring time ];
  homepage = "https://github.com/haskell/unix";
  description = "POSIX functionality";
  license = lib.licenses.bsd3;
}
