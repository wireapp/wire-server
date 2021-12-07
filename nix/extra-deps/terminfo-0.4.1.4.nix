{ mkDerivation, base, lib, ncurses }:
mkDerivation {
  pname = "terminfo";
  version = "0.4.1.4";
  sha256 = "10bba4339f497d01efb7d0190b6842567050efad48ece4f933f3326f28b6179c";
  revision = "1";
  editedCabalFile = "0f82h8mj3swx7c2cxls76nzqx0qnibvsncmvqcbc7v5db4mkfmm1";
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ ncurses ];
  homepage = "https://github.com/judah/terminfo";
  description = "Haskell bindings to the terminfo library";
  license = lib.licenses.bsd3;
}
