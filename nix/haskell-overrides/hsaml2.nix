{ mkDerivation, asn1-encoding, asn1-types, base, base64-bytestring
, bytestring, cryptonite, data-default, fetchgit, http-types, HUnit
, hxt, hxt-charproperties, hxt-http, hxt-unicode, invertible
, invertible-hxt, lens, lib, libxml2, memory, mtl, network-uri
, process, semigroups, silently, string-conversions
, template-haskell, time, utf8-string, x509, zlib
}:
mkDerivation {
  pname = "hsaml2";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/wireapp/hsaml2";
    sha256 = "16hj3i4h5rwhr8kqrs7345wg7v10ahwjd3fdp2qx3c5z4qls6prr";
    rev = "d43818aac56678f0be02d0101d224fe0f6cdf131";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    asn1-encoding asn1-types base base64-bytestring bytestring
    cryptonite data-default http-types hxt hxt-charproperties
    hxt-unicode invertible invertible-hxt lens memory mtl network-uri
    process semigroups silently string-conversions template-haskell
    time utf8-string x509 zlib
  ];
  libraryPkgconfigDepends = [ libxml2 ];
  testHaskellDepends = [
    asn1-encoding asn1-types base base64-bytestring bytestring
    cryptonite data-default http-types HUnit hxt hxt-charproperties
    hxt-http hxt-unicode invertible invertible-hxt lens memory mtl
    network-uri process semigroups silently string-conversions
    template-haskell time utf8-string x509 zlib
  ];
  testPkgconfigDepends = [ libxml2 ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/dylex/hsaml2#readme";
  description = "OASIS Security Assertion Markup Language (SAML) V2.0";
  license = lib.licenses.asl20;
}
