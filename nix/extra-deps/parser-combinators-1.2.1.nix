{ mkDerivation, base, lib }:
mkDerivation {
  pname = "parser-combinators";
  version = "1.2.1";
  sha256 = "03162e40cde50253529fa452165b681d5064d03ad07992800702156adfb6254d";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/mrkkrp/parser-combinators";
  description = "Lightweight package providing commonly useful parser combinators";
  license = lib.licenses.bsd3;
}
