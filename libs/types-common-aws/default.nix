{ mkDerivation, amazonka, amazonka-sqs, base, base64-bytestring
, exceptions, imports, lens, lib, monad-control, proto-lens
, resourcet, safe, tasty, tasty-hunit, text, time
}:
mkDerivation {
  pname = "types-common-aws";
  version = "0.16.0";
  src = ./.;
  libraryHaskellDepends = [
    amazonka amazonka-sqs base base64-bytestring exceptions imports
    lens monad-control proto-lens resourcet safe tasty tasty-hunit text
    time
  ];
  description = "Shared AWS type definitions";
  license = lib.licenses.agpl3Only;
}
