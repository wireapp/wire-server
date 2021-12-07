{ mkDerivation, base, containers, haskell-src-exts, HUnit, lib
, pretty, syb, tasty, tasty-hunit, template-haskell, th-orphans
}:
mkDerivation {
  pname = "haskell-src-meta";
  version = "0.8.5";
  sha256 = "331271554ba42e4938da1417df1e3192737c3e19dceb9a5484198c71ecb858b3";
  revision = "1";
  editedCabalFile = "00znr8mrlbyn0n1bw4c82rv82pq5ngkk7kw9cgk13pghf93hwwv7";
  libraryHaskellDepends = [
    base haskell-src-exts pretty syb template-haskell th-orphans
  ];
  testHaskellDepends = [
    base containers haskell-src-exts HUnit pretty syb tasty tasty-hunit
    template-haskell
  ];
  description = "Parse source to template-haskell abstract syntax";
  license = lib.licenses.bsd3;
}
