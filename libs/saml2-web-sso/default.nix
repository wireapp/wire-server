# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, asn1-encoding
, asn1-parse
, asn1-types
, base
, base64-bytestring
, binary
, bytestring
, case-insensitive
, containers
, cookie
, crypton
, crypton-x509
, data-default
, directory
, dns
, email-validate
, errors
, exceptions
, extra
, file-path-th
, filepath
, foundation
, ghc-prim
, gitignoreSource
, hedgehog
, hedgehog-quickcheck
, hourglass
, hsaml2
, hspec
, hspec-core
, hspec-discover
, hspec-wai
, http-media
, http-types
, hxt
, lens
, lens-datetime
, lib
, memory
, mtl
, network-uri
, pretty-show
, process
, QuickCheck
, quickcheck-instances
, random
, servant
, servant-multipart
, servant-server
, shelly
, silently
, string-conversions
, temporary
, text
, time
, transformers
, uniplate
, uri-bytestring
, uuid
, wai
, wai-extra
, wai-utilities
, warp
, word8
, xml-conduit
, xml-conduit-writer
, xml-hamlet
, xml-types
, yaml
}:
mkDerivation {
  pname = "saml2-web-sso";
  version = "0.20";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    asn1-encoding
    asn1-parse
    asn1-types
    base
    base64-bytestring
    binary
    bytestring
    case-insensitive
    containers
    cookie
    crypton
    crypton-x509
    data-default
    directory
    dns
    email-validate
    errors
    exceptions
    extra
    file-path-th
    filepath
    foundation
    ghc-prim
    hedgehog
    hedgehog-quickcheck
    hourglass
    hsaml2
    hspec
    hspec-wai
    http-media
    http-types
    hxt
    lens
    lens-datetime
    memory
    mtl
    network-uri
    pretty-show
    process
    QuickCheck
    quickcheck-instances
    random
    servant
    servant-multipart
    servant-server
    shelly
    silently
    string-conversions
    temporary
    text
    time
    transformers
    uniplate
    uri-bytestring
    uuid
    wai
    wai-extra
    warp
    word8
    xml-conduit
    xml-conduit-writer
    xml-hamlet
    xml-types
    yaml
  ];
  executableHaskellDepends = [
    aeson
    asn1-encoding
    asn1-parse
    asn1-types
    base
    base64-bytestring
    binary
    bytestring
    case-insensitive
    containers
    cookie
    crypton
    crypton-x509
    data-default
    directory
    dns
    email-validate
    errors
    exceptions
    extra
    filepath
    foundation
    ghc-prim
    hedgehog
    hedgehog-quickcheck
    hourglass
    hsaml2
    hspec
    hspec-wai
    http-media
    http-types
    hxt
    lens
    lens-datetime
    memory
    mtl
    network-uri
    pretty-show
    process
    QuickCheck
    quickcheck-instances
    random
    servant
    servant-multipart
    servant-server
    shelly
    silently
    string-conversions
    temporary
    text
    time
    transformers
    uniplate
    uri-bytestring
    uuid
    wai
    wai-extra
    wai-utilities
    warp
    word8
    xml-conduit
    xml-conduit-writer
    xml-hamlet
    xml-types
    yaml
  ];
  testHaskellDepends = [
    aeson
    asn1-encoding
    asn1-parse
    asn1-types
    base
    base64-bytestring
    binary
    bytestring
    case-insensitive
    containers
    cookie
    crypton
    crypton-x509
    data-default
    directory
    dns
    email-validate
    errors
    exceptions
    extra
    filepath
    foundation
    ghc-prim
    hedgehog
    hedgehog-quickcheck
    hourglass
    hsaml2
    hspec
    hspec-core
    hspec-discover
    hspec-wai
    http-media
    http-types
    hxt
    lens
    lens-datetime
    memory
    mtl
    network-uri
    pretty-show
    process
    QuickCheck
    quickcheck-instances
    random
    servant
    servant-multipart
    servant-server
    shelly
    silently
    string-conversions
    temporary
    text
    time
    transformers
    uniplate
    uri-bytestring
    uuid
    wai
    wai-extra
    warp
    word8
    xml-conduit
    xml-conduit-writer
    xml-hamlet
    xml-types
    yaml
  ];
  testToolDepends = [ hspec-discover ];
  description = "Library and example web app for the SAML Web-based SSO profile";
  license = lib.licenses.agpl3Only;
  mainProgram = "toy-sp";
}
