{ mkDerivation, aeson, asn1-encoding, asn1-parse, asn1-types, base
, base64-bytestring, binary, bytestring, case-insensitive
, containers, cookie, cryptonite, data-default, directory, dns
, email-validate, errors, exceptions, extra, fetchgit, filepath
, foundation, ghc-prim, hedgehog, hedgehog-quickcheck, hourglass
, hpack, hsaml2, hspec, hspec-core, hspec-discover, hspec-wai
, http-media, http-types, hxt, lens, lens-datetime, lib, memory
, mtl, network-uri, pretty-show, process, QuickCheck
, quickcheck-instances, random, servant, servant-multipart
, servant-server, shelly, silently, string-conversions, temporary
, text, time, transformers, uniplate, uri-bytestring, uuid, wai
, wai-extra, warp, word8, x509, xml-conduit, xml-conduit-writer
, xml-hamlet, xml-types, yaml
}:
mkDerivation {
  pname = "saml2-web-sso";
  version = "0.18";
  src = fetchgit {
    url = "https://github.com/wireapp/saml2-web-sso";
    sha256 = "0x1da015vkv7jrdk6rdnw6yy2kb25mkl4lci8ln0lsjkqp406hk2";
    rev = "b8c0bad8e6297c2cfcd608d0f2f89a6129d16361";
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
  libraryToolDepends = [ hpack ];
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
  prePatch = "hpack";
  description = "Library and example web app for the SAML Web-based SSO profile";
  license = lib.licenses.agpl3Only;
}
