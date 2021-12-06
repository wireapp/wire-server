{ mkDerivation, asn1-encoding, asn1-types, base, base64-bytestring
, bytestring, cryptonite, data-default, fetchgit, hpack, http-types
, HUnit, hxt, hxt-charproperties, hxt-http, hxt-unicode, invertible
, invertible-hxt, lens, lib, libxml2, memory, mtl, network-uri
, process, semigroups, silently, string-conversions
, template-haskell, time, utf8-string, x509, zlib
}:
mkDerivation {
  pname = "hsaml2";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/wireapp/hsaml2";
    sha256 = "1y5ibjca4dbqknpr902rrjj94cklwrnl353y6kq5fdxl93v3kdq5";
    rev = "b652ec6e69d1647e827cbee0fa290605ac09dc63";
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
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    asn1-encoding asn1-types base base64-bytestring bytestring
    cryptonite data-default http-types HUnit hxt hxt-charproperties
    hxt-http hxt-unicode invertible invertible-hxt lens memory mtl
    network-uri process semigroups silently string-conversions
    template-haskell time utf8-string x509 zlib
  ];
  testPkgconfigDepends = [ libxml2 ];
  prePatch = "hpack";
  homepage = "https://github.com/dylex/hsaml2#readme";
  description = "OASIS Security Assertion Markup Language (SAML) V2.0";
  license = lib.licenses.asl20;
}
