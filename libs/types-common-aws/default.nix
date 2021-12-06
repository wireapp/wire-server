{ mkDerivation, amazonka, amazonka-sqs, base, base64-bytestring
, exceptions, hpack, imports, lens, lib, monad-control, proto-lens
, safe, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "types-common-aws";
  version = "0.16.0";
  src = /home/axeman/workspace/wire-server/libs/types-common-aws;
  libraryHaskellDepends = [
    amazonka amazonka-sqs base base64-bytestring exceptions imports
    lens monad-control proto-lens safe tasty tasty-hunit text
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "Shared AWS type definitions";
  license = lib.licenses.agpl3Only;
}
