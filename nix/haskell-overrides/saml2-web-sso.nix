{ mkDerivation, aeson, asn1-encoding, asn1-parse, asn1-types, base
, base64-bytestring, binary, bytestring, case-insensitive
, containers, cookie, cryptonite, data-default, directory, dns
, email-validate, errors, exceptions, extra, fetchgit, filepath
, foundation, ghc-prim, hedgehog, hedgehog-quickcheck, hourglass
, hsaml2, hspec, hspec-core, hspec-discover, hspec-wai, http-media
, http-types, hxt, lens, lens-datetime, lib, memory, mtl
, network-uri, pretty-show, process, QuickCheck
, quickcheck-instances, random, servant, servant-multipart
, servant-server, shelly, silently, string-conversions, temporary
, text, time, transformers, uniplate, uri-bytestring, uuid, wai
, wai-extra, warp, word8, x509, xml-conduit, xml-conduit-writer
, xml-hamlet, xml-types, yaml
}:
mkDerivation {
  pname = "saml2-web-sso";
  version = "0.19";
  src = fetchgit {
    url = "https://github.com/wireapp/saml2-web-sso";
    sha256 = "1w23yz2iiayniymk7k4g8gww7268187cayw0c8m3bz2hbnvbyfbc";
    rev = "74371cd775cb98d6cf85f6e182244a3c4fd48702";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson asn1-encoding asn1-parse asn1-types base base64-bytestring
    binary bytestring case-insensitive containers cookie cryptonite
    data-default directory dns email-validate errors exceptions extra
    filepath foundation ghc-prim hedgehog hedgehog-quickcheck hourglass
    hsaml2 hspec hspec-wai http-media http-types hxt lens lens-datetime
    memory mtl network-uri pretty-show process QuickCheck
    quickcheck-instances random servant servant-multipart
    servant-server shelly silently string-conversions temporary text
    time transformers uniplate uri-bytestring uuid wai wai-extra warp
    word8 x509 xml-conduit xml-conduit-writer xml-hamlet xml-types yaml
  ];
  executableHaskellDepends = [
    aeson asn1-encoding asn1-parse asn1-types base base64-bytestring
    binary bytestring case-insensitive containers cookie cryptonite
    data-default directory dns email-validate errors exceptions extra
    filepath foundation ghc-prim hedgehog hedgehog-quickcheck hourglass
    hsaml2 hspec hspec-wai http-media http-types hxt lens lens-datetime
    memory mtl network-uri pretty-show process QuickCheck
    quickcheck-instances random servant servant-multipart
    servant-server shelly silently string-conversions temporary text
    time transformers uniplate uri-bytestring uuid wai wai-extra warp
    word8 x509 xml-conduit xml-conduit-writer xml-hamlet xml-types yaml
  ];
  testHaskellDepends = [
    aeson asn1-encoding asn1-parse asn1-types base base64-bytestring
    binary bytestring case-insensitive containers cookie cryptonite
    data-default directory dns email-validate errors exceptions extra
    filepath foundation ghc-prim hedgehog hedgehog-quickcheck hourglass
    hsaml2 hspec hspec-core hspec-discover hspec-wai http-media
    http-types hxt lens lens-datetime memory mtl network-uri
    pretty-show process QuickCheck quickcheck-instances random servant
    servant-multipart servant-server shelly silently string-conversions
    temporary text time transformers uniplate uri-bytestring uuid wai
    wai-extra warp word8 x509 xml-conduit xml-conduit-writer xml-hamlet
    xml-types yaml
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  description = "Library and example web app for the SAML Web-based SSO profile";
  license = lib.licenses.agpl3Only;
  mainProgram = "toy-sp";
}
