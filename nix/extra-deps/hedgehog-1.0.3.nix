{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, fail, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, semigroups, stm
, template-haskell, text, time, transformers, transformers-base
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0.3";
  sha256 = "35f9e96578c09734243a4e423d367083e08937f863101b3f6ef48744c8299b82";
  revision = "1";
  editedCabalFile = "0v0wgij1sz7smmh7j74g8jdi6gvjdv90f20jci7qskx38mhn5n0j";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    deepseq directory erf exceptions fail lifted-async mmorph
    monad-control mtl pretty-show primitive random resourcet semigroups
    stm template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
