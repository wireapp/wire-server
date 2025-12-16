module Test.Wire.API.Golden.Manual.IdP where

import Data.Id
import Data.List.NonEmpty
import Data.UUID
import Imports
import SAML2.WebSSO.Types
import Text.XML.DSig
import URI.ByteString
import Wire.API.Routes.Version
import Wire.API.User.IdentityProvider

testObject_IdP_1 :: IdP
testObject_IdP_1 =
  IdPConfig
    { _idpId = IdPId {fromIdPId = (fromJust . Data.UUID.fromString) "614c0bb0-1b33-98b6-8600-a1b290bbe1d7"},
      _idpMetadata =
        IdPMetadata
          { _edIssuer = Issuer {_fromIssuer = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "liisa.kaisa"}, authorityPort = Nothing}), uriPath = "/", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}},
            _edRequestURI = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "johanna.leks"}, authorityPort = Nothing}), uriPath = "/aytamah", uriQuery = Query {queryPairs = []}, uriFragment = Nothing},
            _edCertAuthnResponse = either error id (parseKeyInfo False "<ds:KeyInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</ds:X509Certificate></ds:X509Data></ds:KeyInfo>") :| []
          },
      _idpExtraInfo =
        WireIdP
          { _team = (either error id . parseIdFromText) "fc5f3bf8-c296-69e7-27fd-70d483740fe4",
            _apiVersion = Nothing,
            _oldIssuers = [Issuer {_fromIssuer = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "hele.johanna"}, authorityPort = Nothing}), uriPath = "/", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}}, Issuer {_fromIssuer = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "ulli.jannis"}, authorityPort = Nothing}), uriPath = "/", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}}, Issuer {_fromIssuer = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "reet.loviise"}, authorityPort = Nothing}), uriPath = "/", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}}],
            _replacedBy = Just (IdPId {fromIdPId = (fromJust . Data.UUID.fromString) "fc5f3bf8-c296-69e7-27fd-70d483740fe4"}),
            _handle = IdPHandle {unIdPHandle = "614c0bb0-1b33-98b6-8600-a1b290bbe1d7"},
            _domain = Just "wire.com"
          }
    }

testObject_IdP_2 :: IdP
testObject_IdP_2 =
  IdPConfig
    { _idpId = IdPId {fromIdPId = (fromJust . Data.UUID.fromString) "614c0bb0-1b33-98b6-8600-a1b290bbe1d7"},
      _idpMetadata =
        IdPMetadata
          { _edIssuer = Issuer {_fromIssuer = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "liisa.kaisa"}, authorityPort = Nothing}), uriPath = "/", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}},
            _edRequestURI = URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "johanna.leks"}, authorityPort = Nothing}), uriPath = "/aytamah", uriQuery = Query {queryPairs = []}, uriFragment = Nothing},
            _edCertAuthnResponse = either error id (parseKeyInfo False "<ds:KeyInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</ds:X509Certificate></ds:X509Data></ds:KeyInfo>") :| []
          },
      _idpExtraInfo =
        WireIdP
          { _team = (either error id . parseIdFromText) "fc5f3bf8-c296-69e7-27fd-70d483740fe4",
            _apiVersion = Just WireIdPAPIV2,
            _oldIssuers = [],
            _replacedBy = Nothing,
            _handle = IdPHandle {unIdPHandle = "614c0bb0-1b33-98b6-8600-a1b290bbe1d7"},
            _domain = Nothing
          }
    }
