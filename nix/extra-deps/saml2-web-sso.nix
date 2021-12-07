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
    sha256 = "0agdvnn20r5bdf769msrqi4zm44i0s5y574nzqqrs894bzim24fm";
    rev = "4227e38be5c0810012dc472fc6931f6087fbce68";
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
