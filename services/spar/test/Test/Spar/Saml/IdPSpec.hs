module Test.Spar.Saml.IdPSpec where

import Arbitrary ()
import Control.Lens hiding (Level, elements)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (..))
import Data.Domain
import Data.Id (TeamId, idToText, parseIdFromText)
import qualified Data.List.NonEmpty as NonEmptyL
import qualified Data.Map as Map
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time (UTCTime (..), fromGregorian)
import Data.X509 (SignedCertificate)
import Data.X509.Extended (renderFingerprintHex)
import qualified Data.X509.Extended as X509E
import Imports
import Polysemy
import qualified Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.State
import Polysemy.TinyLog
import SAML2.WebSSO hiding (authresp)
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Arbitrary (mkArbitrarySignedCert)
import Servant.Multipart (Mem, MultipartData (..), fromMultipart)
import Servant.Server (ServerError (..))
import Spar.API (authresp, idpCreate, idpCreateV7, idpDelete, idpUpdate)
import Spar.Error
import Spar.Options (CertFingerprintAllowlist (CertFingerprintAllowlist))
import qualified Spar.Options
import Spar.Sem.AReqIDStore (AReqIDStore (..))
import Spar.Sem.AssIDStore (AssIDStore (..))
import Spar.Sem.IdPRawMetadataStore
import Spar.Sem.IdPRawMetadataStore.Mem
import Spar.Sem.Reporter (Reporter (..))
import Spar.Sem.SAML2 (SAML2 (..))
import Spar.Sem.SAMLUserStore
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import Spar.Sem.SAMLUserStore.Mem
import Spar.Sem.SamlProtocolSettings (SamlProtocolSettings)
import Spar.Sem.SamlProtocolSettings.Servant (sparRouteToServant)
import Spar.Sem.ScimTokenStore
import Spar.Sem.ScimTokenStore.Mem
import qualified Spar.Sem.VerdictFormatStore as VerdictFormatStore
import System.FilePath ((</>))
import System.Logger (Msg)
import System.Logger.Class (Level (..))
import Test.Hspec
import Test.QuickCheck
import qualified Text.XML.DSig as DSig
import URI.ByteString (parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri)
import qualified Util.Options
import Web.Cookie (defaultSetCookie)
import Wire.API.Routes.Internal.Brig (IdpChangedNotification (..))
import Wire.API.Team.Feature (FeatureStatus (FeatureStatusEnabled), LockableFeature (..))
import Wire.API.Team.Member (mkNewTeamMember, ntmNewTeamMember, rolePermissions)
import Wire.API.Team.Role (Role (RoleOwner))
import Wire.API.User (User (..), userId)
import Wire.API.User.IdentityProvider (IdP, IdPMetadataInfo (..), WireIdPAPIVersion (..), oldIssuers, replacedBy, team)
import Wire.API.User.Saml (TTL (..), VerdictFormat (..))
import Wire.BrigAPIAccess (BrigAPIAccess)
import qualified Wire.BrigAPIAccess
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import qualified Wire.GalleyAPIAccess
import Wire.IdPConfigStore
import Wire.IdPConfigStore.Mem
import Wire.Sem.Logger (discardLogs)
import Wire.Sem.Logger.TinyLog (LogRecorder (..), newLogRecorder, recordLogs)
import Wire.Sem.Random
import Wire.Sem.Random.Null

spec :: Spec
spec =
  let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
      zUser = either error Just $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
      anyMultiIngressDomainCfg =
        MultiIngressDomainConfig
          { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
            _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
            _cfgContacts = [fallbackContact]
          }
      singleIngressSamlConfig =
        Config
          { -- The log level only matters for log output, not production.
            -- Thus, we could put anything here, it just needs to be a valid
            -- value.
            _cfgLogLevel = Trace,
            _cfgSPHost = "localhost",
            _cfgSPPort = 8081,
            _cfgDomainConfigs = Left anyMultiIngressDomainCfg
          }
      host = Just "backend.example.com"
      miHost1AsText = "backend-1.example.com"
      miDomain1 = either (error . show) id $ mkDomain miHost1AsText
      miHost1 = Just miHost1AsText
      miHost2AsText = "backend-2.example.com"
      miDomain2 = either (error . show) id $ mkDomain miHost2AsText
      miHost2 = Just miHost2AsText
      multiIngressSamlConfig =
        Config
          { -- The log level only matters for log output, not production.
            -- Thus, we could put anything here, it just needs to be a valid
            -- value.
            _cfgLogLevel = Trace,
            _cfgSPHost = "localhost",
            _cfgSPPort = 8081,
            _cfgDomainConfigs =
              Right $
                Map.fromList [(miDomain1, anyMultiIngressDomainCfg), (miDomain2, anyMultiIngressDomainCfg)]
          }
      idpHandle = Just $ unsafeRange "some-idp"
      apiVersionV2 = Just WireIdPAPIV2
      issuerString = "https://accounts.accesscontrol.windows.net/auth"
      issuer =
        either (error . show) Issuer
          . parseURI strictURIParserOptions
          . fromString
          $ issuerString
      idpEndpointString = "https://idp-endpoint.example.com"
      idpEndpoint =
        either (error . show) id
          . parseURI strictURIParserOptions
          . fromString
          $ idpEndpointString
   in do
        describe "SAML IdP change logging" $ do
          describe "idp-create" $ do
            it "should log IdP creation" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP created, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain=None, user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", replaces=None"
                        <> "\n"
                    )

              forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                (logs, _notifs, _res) <-
                  interpretWithLoggingMock
                    Nothing
                    (idpCreate singleIngressSamlConfig tid zUser host idPMetadataInfo' Nothing (Just apiVersion) idpHandle)
                logs `shouldContain` [expectedLogLine]

                (logsV7, _notifs, _res) <-
                  interpretWithLoggingMock
                    Nothing
                    (idpCreateV7 singleIngressSamlConfig tid zUser idPMetadataInfo' Nothing (Just apiVersion) idpHandle)
                logsV7 `shouldContain` [expectedLogLine]

            it "should log IdP creation with domain for multi-ingress" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine :: LByteString -> LogLine
                  expectedLogLine domainPart =
                    ( Info,
                      "IdP created, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain="
                        <> domainPart
                        <> ", user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", replaces=None"
                        <> "\n"
                    )
                  expectedLogLineWithDomain = expectedLogLine . TL.encodeUtf8 . TL.fromStrict $ miHost1AsText
                  expectedLogLineWithoutDomain = expectedLogLine "None"

              forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                (logs, _notifs, _res) <-
                  interpretWithLoggingMock
                    Nothing
                    (idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo' Nothing (Just apiVersion) idpHandle)
                logs `shouldContain` [expectedLogLineWithDomain]

                -- >=V7 does not bother with multi-ingress domains for IdPs as it can
                -- only have one IdP per team anyways.
                (logsV7, _notifs, _res) <-
                  interpretWithLoggingMock
                    Nothing
                    (idpCreateV7 multiIngressSamlConfig tid zUser idPMetadataInfo' Nothing (Just apiVersion) idpHandle)
                logsV7 `shouldContain` [expectedLogLineWithoutDomain]

          describe "idp-delete" $ do
            it "should log IdP deletion" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP deleted, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain=None, user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate singleIngressSamlConfig tid zUser host idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpDelete singleIngressSamlConfig zUser (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]

            it "should log IdP deletion with domain for multi-ingress" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP deleted, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain="
                        <> (TL.encodeUtf8 . TL.fromStrict) miHost1AsText
                        <> ", user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpDelete multiIngressSamlConfig zUser (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]

          describe "idp-update" $ do
            it "should log IdP update" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP updated, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain=None, user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate singleIngressSamlConfig tid zUser host idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpUpdate singleIngressSamlConfig zUser host idPMetadataInfo' (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]

            it "should log IdP update with domain for multi-ingress" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP updated, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", domain="
                        <> (TL.encodeUtf8 . TL.fromStrict) miHost1AsText
                        <> ", user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpUpdate multiIngressSamlConfig zUser miHost1 idPMetadataInfo' (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]

            it "should log IdP update with changed domain for multi-ingress" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              let idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  expectedLogLine =
                    ( Info,
                      "IdP updated, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000, issuer="
                        <> fromString issuerString
                        <> ", old-domain="
                        <> (TL.encodeUtf8 . TL.fromStrict) miHost1AsText
                        <> ", new-domain="
                        <> (TL.encodeUtf8 . TL.fromStrict) miHost2AsText
                        <> ", user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpUpdate multiIngressSamlConfig zUser miHost2 idPMetadataInfo' (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]

            it "should log IdP update (changed cert)" $ do
              idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
              user :: User <- generate arbitrary
              newKeyInfo <- readSampleIO "okta-keyinfo-1.xml"
              let newIssuerString = "https://new.idp.example.com/auth"
                  newIssuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ newIssuerString
                  newIdpEndpointString = "https://new.idp.example.com/login"
                  newRequestURI = either (error . show) id . parseURI strictURIParserOptions . fromString $ newIdpEndpointString
                  idPMetadataInfo' =
                    idPMetadataInfo
                      { _idpMetadataRecord =
                          (idPMetadataInfo._idpMetadataRecord)
                            { SAML._edIssuer = issuer,
                              SAML._edRequestURI = idpEndpoint
                            }
                      }

                  newCert = either (error . show) id $ DSig.parseKeyInfo False newKeyInfo
                  newIdPMetadata :: IdPMetadata =
                    IdPMetadata
                      { _edIssuer = newIssuer,
                        _edRequestURI = newRequestURI,
                        _edCertAuthnResponse = NonEmptyL.singleton newCert
                      }
                  idPMetadataInfo'' = IdPMetadataValue ((TL.toStrict . encode) newIdPMetadata) newIdPMetadata
                  expectedLogLine =
                    ( Info,
                      "IdP updated, team="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText) tid
                        <> ", idpId=00000000-0000-0000-0000-000000000000"
                        <> ", old-issuer="
                        <> fromString issuerString
                        <> ", new-issuer="
                        <> fromString newIssuerString
                        <> ", domain=None, user="
                        <> (TL.encodeUtf8 . TL.fromStrict . idToText . fromJust) zUser
                        <> ", old-idp-endpoint="
                        <> fromString idpEndpointString
                        <> ", new-idp-endpoint="
                        <> fromString newIdpEndpointString
                        <> ", certificates=Issuer: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; Subject: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; SHA1 Fingerprint: 5C:42:5B:27:B3:96:CC:9D:1B:1F:0E:4F:2B:8A:B8:E4:3C:9E:96:34"
                        <> ", new-certificates=Issuer: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; Subject: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; SHA1 Fingerprint: 5C:42:5B:27:B3:96:CC:9D:1B:1F:0E:4F:2B:8A:B8:E4:3C:9E:96:34"
                        <> ", removed-certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37"
                        <> "\n"
                    )

              (logs, _notifs, _res) <- interpretWithLoggingMock (Just user) $ do
                idp <- idpCreate singleIngressSamlConfig tid zUser host idPMetadataInfo' Nothing apiVersionV2 idpHandle
                idpUpdate singleIngressSamlConfig zUser host idPMetadataInfo'' (idp._idpId) Nothing
              logs `shouldContain` [expectedLogLine]
        describe "SAML IdP change notification emails" $ do
          context "when multi-ingress is configured" $ do
            describe "idp-create" $ do
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary

                forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                  (_logs, notifs, idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifs `shouldBe` [IdPCreated zUser idp]

                  (_logs, notifsV7, idpV7) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreateV7 multiIngressSamlConfig tid zUser idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifsV7 `shouldBe` [IdPCreated zUser idpV7]

              it "should send without zUser if none is given" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary

                forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                  (_logs, notifs, idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreate multiIngressSamlConfig tid Nothing miHost1 idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifs `shouldBe` [IdPCreated Nothing idp]

                  (_logs, notifsV7, idpV7) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreateV7 multiIngressSamlConfig tid Nothing idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifsV7 `shouldBe` [IdPCreated Nothing idpV7]

            describe "idp-delete" $ do
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, idp) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  void $ idpDelete multiIngressSamlConfig zUser (idp._idpId) Nothing
                  pure idp
                notifs `shouldBe` [IdPDeleted (fromJust zUser) idp, IdPCreated zUser idp]

            describe "idp-update" $ do
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, (oldIdP, newIdP)) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate multiIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  updatedIdP <- idpUpdate multiIngressSamlConfig zUser miHost1 idPMetadataInfo (idp._idpId) Nothing
                  pure (idp, updatedIdP)
                notifs `shouldBe` [IdPUpdated (fromJust zUser) oldIdP newIdP, IdPCreated zUser oldIdP]

          context "when multi-ingress is NOT configured (common case)" $ do
            describe "idp-create" $ do
              it "should NOT send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary

                forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                  (_logs, notifs, _idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifs `shouldBe` mempty

                  (_logs, notifsV7, _idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreateV7 singleIngressSamlConfig tid zUser idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifsV7 `shouldBe` mempty

            describe "idp-delete" $ do
              it "should NOT send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, _) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  idpDelete singleIngressSamlConfig zUser (idp._idpId) Nothing
                notifs `shouldBe` mempty

            describe "idp-update" $ do
              it "should NOT send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, _) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  idpUpdate singleIngressSamlConfig zUser miHost1 idPMetadataInfo (idp._idpId) Nothing
                notifs `shouldBe` mempty

        describe "IdP cert fingerprint allowlist" $ do
          let withAllow :: Maybe Spar.Options.CertFingerprintAllowlist -> Spar.Options.Opts
              withAllow allow = defaultTestOpts {Spar.Options.idpCertFingerprintAllowlist = allow}

              generateArbitraryIdPInfo :: IO IdPMetadataInfo
              generateArbitraryIdPInfo = do
                IdPMetadataValue rawXml idpMeta <- generate arbitrary
                n <- generate $ choose (1, 3 :: Int)
                certs <- fmap NonEmptyL.fromList $ generate $ replicateM n mkArbitrarySignedCert
                pure $ IdPMetadataValue rawXml (idpMeta {_edCertAuthnResponse = certs})

              generateTwoCertIdPInfo :: IO IdPMetadataInfo
              generateTwoCertIdPInfo = do
                IdPMetadataValue rawXml idpMeta <- generate arbitrary
                certs <- fmap NonEmptyL.fromList $ generate $ replicateM 2 mkArbitrarySignedCert
                pure $ IdPMetadataValue rawXml (idpMeta {_edCertAuthnResponse = certs})

              firstCertFingerprint :: IdPMetadataInfo -> X509E.Fingerprint
              firstCertFingerprint (IdPMetadataValue _ idpMeta) =
                X509E.certSha1Fingerprint . NonEmptyL.head $
                  idpMeta._edCertAuthnResponse

              allCertsAllowlist :: IdPMetadataInfo -> Spar.Options.CertFingerprintAllowlist
              allCertsAllowlist (IdPMetadataValue _ idpMeta) =
                Spar.Options.CertFingerprintAllowlist $
                  Set.fromList $
                    map X509E.certSha1Fingerprint $
                      NonEmptyL.toList idpMeta._edCertAuthnResponse

              singletonAllowlist :: X509E.Fingerprint -> Spar.Options.CertFingerprintAllowlist
              singletonAllowlist = Spar.Options.CertFingerprintAllowlist . Set.singleton

              bogusFingerprint :: X509E.Fingerprint
              bogusFingerprint =
                either (error "impossible: zero fingerprint failed to parse") id $
                  X509E.parseFingerprintHex
                    "0000000000000000000000000000000000000000"

              certToFingerprint :: SignedCertificate -> TL.Text
              certToFingerprint =
                TL.fromStrict
                  . renderFingerprintHex
                  . X509E.certSha1Fingerprint

              runCreate :: Spar.Options.Opts -> IdPMetadataInfo -> IO ([LogLine], Either SparError IdP)
              runCreate opts idPMetadataInfo = do
                (logs, _notifs, res) <-
                  interpretWithLoggingMockOptsE opts Nothing $
                    idpCreate singleIngressSamlConfig tid zUser host idPMetadataInfo Nothing apiVersionV2 idpHandle
                pure (logs, res)

              runCreateUpdate :: Spar.Options.Opts -> IdPMetadataInfo -> IdPMetadataInfo -> IO ([LogLine], Either SparError IdP)
              runCreateUpdate opts createInfo updateInfo = do
                (logs, _notifs, res) <-
                  interpretWithLoggingMockOptsE opts Nothing $ do
                    idp <- idpCreate singleIngressSamlConfig tid zUser host createInfo Nothing apiVersionV2 idpHandle
                    idpUpdate singleIngressSamlConfig zUser host updateInfo (idp._idpId) Nothing
                pure (logs, res)

          describe "create" $ do
            it "accepts any cert when allowlist is Nothing" $ do
              idpInfo <- generateArbitraryIdPInfo
              (_logs, res) <- runCreate (withAllow Nothing) idpInfo
              res `shouldSatisfy` isRight

            it "accepts any cert when allowlist is empty" $ do
              idpInfo <- generateArbitraryIdPInfo
              let empty = Spar.Options.CertFingerprintAllowlist Set.empty
              (_logs, res) <- runCreate (withAllow (Just empty)) idpInfo
              res `shouldSatisfy` isRight

            it "accepts when all fingerprints are allowlisted" $ do
              idpInfo <- generateArbitraryIdPInfo
              let allow = allCertsAllowlist idpInfo
              (_logs, res) <- runCreate (withAllow (Just allow)) idpInfo
              res `shouldSatisfy` isRight

            it "accepts when all multi-cert fingerprints are allowlisted" $ do
              idpInfo <- generateTwoCertIdPInfo
              let allow = allCertsAllowlist idpInfo
              (_logs, res) <- runCreate (withAllow (Just allow)) idpInfo
              res `shouldSatisfy` isRight

            it "rejects when no fingerprint matches and logs the refusal" $ do
              idpInfo@(IdPMetadataValue _ m) <- generateArbitraryIdPInfo
              let allow = singletonAllowlist bogusFingerprint
                  fingerprint = certToFingerprint . NonEmptyL.head $ m._edCertAuthnResponse
              (logs, res) <- runCreate (withAllow (Just allow)) idpInfo
              res `shouldBe` Left (SAML.CustomError (SparIdPCertNotAllowed fingerprint))
              let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
              logged `shouldSatisfy` (("cert fingerprint not in allowlist, fingerprint=" <> fingerprint) `TL.isInfixOf`)

            it "rejects when any fingerprint in multi-cert descriptor is not allowlisted" $ do
              idpInfo@(IdPMetadataValue _ m) <- generateTwoCertIdPInfo
              let allow = singletonAllowlist (firstCertFingerprint idpInfo)
                  secondCertFingerprint = certToFingerprint . head . NonEmptyL.tail $ m._edCertAuthnResponse
              (logs, res) <- runCreate (withAllow (Just allow)) idpInfo
              res `shouldBe` Left (SAML.CustomError (SparIdPCertNotAllowed secondCertFingerprint))
              let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
              logged `shouldSatisfy` (("cert fingerprint not in allowlist, fingerprint=" <> secondCertFingerprint) `TL.isInfixOf`)

          describe "update" $ do
            it "accepts any cert when allowlist is Nothing" $ do
              idpInfoCrt <- generateArbitraryIdPInfo
              idpInfoUpd <- generateArbitraryIdPInfo
              (_logs, res) <- runCreateUpdate (withAllow Nothing) idpInfoCrt idpInfoUpd
              res `shouldSatisfy` isRight

            it "accepts any cert when allowlist is empty" $ do
              idpInfoCrt <- generateArbitraryIdPInfo
              idpInfoUpd <- generateArbitraryIdPInfo
              (_logs, res) <- runCreateUpdate (withAllow (Just mempty)) idpInfoCrt idpInfoUpd
              res `shouldSatisfy` isRight

            it "accepts when all fingerprints are allowlisted" $ do
              idpInfoCrt <- generateArbitraryIdPInfo
              idpInfoUpd <- generateArbitraryIdPInfo
              let allow = allCertsAllowlist idpInfoCrt <> allCertsAllowlist idpInfoUpd
              (_logs, res) <- runCreateUpdate (withAllow (Just allow)) idpInfoCrt idpInfoUpd
              res `shouldSatisfy` isRight

            it "accepts when all multi-cert fingerprints are allowlisted" $ do
              idpInfoCrt <- generateTwoCertIdPInfo
              idpInfoUpd <- generateTwoCertIdPInfo
              let allow = allCertsAllowlist idpInfoCrt <> allCertsAllowlist idpInfoUpd
              (_logs, res) <- runCreateUpdate (withAllow (Just allow)) idpInfoCrt idpInfoUpd
              res `shouldSatisfy` isRight

            it "rejects when no fingerprint matches" $ do
              idpInfo@(IdPMetadataValue _ m) <- generateArbitraryIdPInfo
              let fingerprint = certToFingerprint . NonEmptyL.head $ m._edCertAuthnResponse
              (_logs1, _notifs1, createdE) <-
                interpretWithLoggingMockOptsE (withAllow Nothing) Nothing $
                  idpCreate singleIngressSamlConfig tid zUser host idpInfo Nothing apiVersionV2 idpHandle
              case createdE of
                Left e -> expectationFailure ("unexpected create failure: " <> show e)
                Right idp -> do
                  let allow = singletonAllowlist bogusFingerprint
                  (_logs2, _notifs2, res) <-
                    interpretWithLoggingMockOptsE (withAllow (Just allow)) Nothing $ do
                      insertConfig idp
                      idpUpdate singleIngressSamlConfig zUser host idpInfo (idp._idpId) Nothing
                  res `shouldBe` Left (SAML.CustomError (SparIdPCertNotAllowed fingerprint))

            it "rejects when any fingerprint in multi-cert descriptor is not allowlisted" $ do
              idpInfo@(IdPMetadataValue _ m) <- generateTwoCertIdPInfo
              let partialAllow = singletonAllowlist (firstCertFingerprint idpInfo)
                  secondCertFingerprint = certToFingerprint . head . NonEmptyL.tail $ m._edCertAuthnResponse
              (_logs1, _notifs1, createdE) <-
                interpretWithLoggingMockOptsE (withAllow Nothing) Nothing $
                  idpCreate singleIngressSamlConfig tid zUser host idpInfo Nothing apiVersionV2 idpHandle
              case createdE of
                Left e -> expectationFailure ("unexpected create failure: " <> show e)
                Right idp -> do
                  (_logs2, _notifs2, res) <-
                    interpretWithLoggingMockOptsE (withAllow (Just partialAllow)) Nothing $ do
                      insertConfig idp
                      idpUpdate singleIngressSamlConfig zUser host idpInfo (idp._idpId) Nothing
                  res `shouldBe` Left (SAML.CustomError (SparIdPCertNotAllowed secondCertFingerprint))

          describe "authresp" $ do
            let makeIdp :: IdPMetadataInfo -> IO IdP
                makeIdp (IdPMetadataValue _ metadata) = do
                  idp <- generate (arbitrary :: Gen IdP)
                  pure $
                    idp
                      & SAML.idpMetadata .~ metadata
                      & SAML.idpExtraInfo . team .~ authrspTestTeamId
                      & SAML.idpExtraInfo . oldIssuers .~ []
                      & SAML.idpExtraInfo . replacedBy .~ Nothing

                makeControlledAssertion :: IO SAML.Assertion
                makeControlledAssertion = do
                  ass <- generate arbitrary
                  let conf =
                        SAML.SubjectConfirmation
                          { SAML._scMethod = SAML.SubjectConfirmationMethodBearer,
                            SAML._scData =
                              Just
                                SAML.SubjectConfirmationData
                                  { SAML._scdNotBefore = Nothing,
                                    SAML._scdNotOnOrAfter = SAML.Time $ UTCTime (fromGregorian 2099 1 1) 0,
                                    SAML._scdRecipient = [uri|https://example-sp.com/sso|],
                                    SAML._scdInResponseTo = Just authrspTestReqId,
                                    SAML._scdAddress = Nothing
                                  }
                          }
                  pure $ ass & SAML.assContents . SAML.sasSubject . SAML.subjectConfirmations .~ [conf]

                makeAuthRespRequest requestParamTeamId idpInfo@(IdPMetadataValue _ m) allow = do
                  idp <- makeIdp idpInfo
                  ass <- makeControlledAssertion
                  raw <- generate (arbitrary :: Gen (MultipartData Mem))
                  let dummyBody = either error id (fromMultipart raw :: Either String SAML.AuthnResponseBody)
                  user <- generate arbitrary
                  let opts = withAllow allow
                      uref = SAML.UserRef m._edIssuer (SAML.unspecifiedNameID "test-user")
                      user' = user {userTeam = Just authrspTestTeamId}
                      verdict = SAML.AccessGranted uref
                  interpretAuthrespE opts (Just user') (ass NonEmptyL.:| [], idp, verdict) $ do
                    SAMLUserStore.insert uref (userId user')
                    authresp requestParamTeamId dummyBody Nothing

            forM_ [Nothing, Just authrspTestTeamId] \requestParamTeamId -> do
              it ("cert allowlisted, AccessGranted → success - teamId param " <> show requestParamTeamId) $ do
                idpInfo <- generateArbitraryIdPInfo
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo $ Just (allCertsAllowlist idpInfo)
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 200
                    errReasonPhrase servantErr `shouldBe` "success"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldNotSatisfy` ("cert fingerprint not in allowlist" `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant (VerifyHandlerGranted), got: " <> show other

              it ("allowlist absent, AccessGranted → success) - teamId param " <> show requestParamTeamId) $ do
                idpInfo <- generateArbitraryIdPInfo
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo Nothing
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 200
                    errReasonPhrase servantErr `shouldBe` "success"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldNotSatisfy` ("cert fingerprint not in allowlist" `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant (VerifyHandlerGranted), got: " <> show other

              it ("allowlist empty, AccessGranted → success - teamId param " <> show requestParamTeamId) $ do
                idpInfo <- generateArbitraryIdPInfo
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo (Just mempty)
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 200
                    errReasonPhrase servantErr `shouldBe` "success"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldNotSatisfy` ("cert fingerprint not in allowlist" `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant (VerifyHandlerGranted), got: " <> show other

              it ("fingerprint not in allowlist, AccessGranted → idp-cert-not-allowed - teamId param " <> show requestParamTeamId) $ do
                idpInfo@(IdPMetadataValue _ m) <- generateArbitraryIdPInfo
                let fingerprint = certToFingerprint . NonEmptyL.head $ m._edCertAuthnResponse
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo $ Just (singletonAllowlist bogusFingerprint)
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 403
                    errReasonPhrase servantErr `shouldBe` "idp-cert-not-allowed"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldSatisfy` (("cert fingerprint not in allowlist, fingerprint=" <> fingerprint) `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant error, got: " <> show other

              it ("one fingerprint of multi-cert not in allowlist, AccessGranted → idp-cert-not-allowed - teamId param " <> show requestParamTeamId) $ do
                idpInfo@(IdPMetadataValue _ m) <- generateTwoCertIdPInfo
                let allow = singletonAllowlist (firstCertFingerprint idpInfo)
                    secondCert = head . NonEmptyL.tail $ m._edCertAuthnResponse
                    secondCertFingerprint = certToFingerprint secondCert
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo $ Just allow
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 403
                    errReasonPhrase servantErr `shouldBe` "idp-cert-not-allowed"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldSatisfy` (("cert fingerprint not in allowlist, fingerprint=" <> secondCertFingerprint) `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant error, got: " <> show other

              it ("all multi-cert fingerprints allowlisted, AccessGranted → success - teamId param " <> show requestParamTeamId) $ do
                idpInfo <- generateTwoCertIdPInfo
                let allow = allCertsAllowlist idpInfo
                (logs, res) <- makeAuthRespRequest requestParamTeamId idpInfo $ Just allow
                case res of
                  Left (SAML.CustomServant servantErr) -> do
                    errHTTPCode servantErr `shouldBe` 200
                    errReasonPhrase servantErr `shouldBe` "success"
                    let logged = TL.decodeUtf8 $ LBS.concat (map snd logs)
                    logged `shouldNotSatisfy` ("cert fingerprint not in allowlist" `TL.isInfixOf`)
                  other -> expectationFailure $ "expected CustomServant (VerifyHandlerGranted), got: " <> show other

type LogLine = (Level, LByteString)

interpretWithLoggingMock ::
  Maybe User ->
  Sem (Effs) a ->
  IO ([LogLine], [IdpChangedNotification], a)
interpretWithLoggingMock = interpretWithLoggingMockOpts defaultTestOpts

interpretWithLoggingMockOpts ::
  Spar.Options.Opts ->
  Maybe User ->
  Sem (Effs) a ->
  IO ([LogLine], [IdpChangedNotification], a)
interpretWithLoggingMockOpts opts mbAccount action = do
  (logs, notifs, res) <- interpretWithLoggingMockOptsE opts mbAccount action
  pure (logs, notifs, either (error . show) id res)

interpretWithLoggingMockOptsE ::
  Spar.Options.Opts ->
  Maybe User ->
  Sem (Effs) a ->
  IO ([LogLine], [IdpChangedNotification], Either SparError a)
interpretWithLoggingMockOptsE opts mbAccount action = do
  lr <- newLogRecorder
  a <-
    runFinal
      . embedToFinal @IO
      . Polysemy.Error.errorToIOFinal
      . recordLogs lr
      . ignoringState idpRawMetadataStoreToMem
      . ignoringState idPToMem
      . ignoringState scimTokenStoreToMem
      . brigAccessMock mbAccount
      . galleyAccessMock
      . ignoringState samlUserStoreToMem
      . randomToNull
      . runInputConst opts
      $ action
  logs <- readIORef lr.recordedLogs
  case a of
    Left sparError -> pure (logs, [], Left sparError)
    Right (notifs, res) -> pure (logs, notifs, Right res)

-- | Test 'Opts' with cert allowlist disabled.  Only the allowlist is read by
-- the code under test; other fields are placeholders.
defaultTestOpts :: Spar.Options.Opts
defaultTestOpts =
  Spar.Options.Opts
    { saml =
        Config
          { _cfgLogLevel = Trace,
            _cfgSPHost = "localhost",
            _cfgSPPort = 8081,
            _cfgDomainConfigs =
              Left
                MultiIngressDomainConfig
                  { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                    _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                    _cfgContacts = [fallbackContact]
                  }
          },
      brig = Util.Options.Endpoint "127.0.0.1" 0,
      galley = Util.Options.Endpoint "127.0.0.1" 0,
      cassandra =
        Util.Options.CassandraOpts
          { endpoint = Util.Options.Endpoint "127.0.0.1" 0,
            keyspace = "",
            filterNodesByDatacentre = Nothing,
            tlsCa = Nothing
          },
      maxttlAuthreq = TTL 5,
      maxttlAuthresp = TTL 7200,
      maxScimTokens = 8,
      richInfoLimit = 5000,
      discoUrl = Nothing,
      logNetStrings = Nothing,
      logFormat = Nothing,
      disabledAPIVersions = mempty,
      scimBaseUri = [uri|http://localhost:8088/scim/v2|],
      enableIdPByEmailDiscovery = False,
      idpCertFingerprintAllowlist = Nothing
    }

galleyAccessMock :: Sem (GalleyAPIAccess ': r) a -> Sem r a
galleyAccessMock = interpret $ \case
  Wire.GalleyAPIAccess.GetTeamMember uid _teamId -> pure (Just $ ntmNewTeamMember $ mkNewTeamMember uid (rolePermissions RoleOwner) Nothing)
  Wire.GalleyAPIAccess.GetFeatureConfigForTeam _teamId -> pure (def {status = FeatureStatusEnabled})
  _ -> undefined

brigAccessMock :: Maybe User -> Sem (BrigAPIAccess ': r) a -> Sem r ([IdpChangedNotification], a)
brigAccessMock mbAccount = (runState @([IdpChangedNotification]) mempty .) $
  reinterpret $ \case
    Wire.BrigAPIAccess.GetAccount _havePendingInvitations _userId -> pure mbAccount
    Wire.BrigAPIAccess.SendSAMLIdPChangedEmail notif -> modify (notif :)
    Wire.BrigAPIAccess.SsoLogin _ _ -> pure defaultSetCookie
    _ -> undefined

ignoringState :: (Functor f) => (a -> f (c, b)) -> a -> f b
ignoringState f = fmap snd . f

type Effs =
  '[ Input Spar.Options.Opts,
     Random,
     SAMLUserStore,
     GalleyAPIAccess,
     BrigAPIAccess,
     ScimTokenStore,
     IdPConfigStore,
     IdPRawMetadataStore,
     Logger (Msg -> Msg),
     Polysemy.Error.Error SparError,
     Embed IO,
     Final IO
   ]

readSampleIO :: (MonadIO m) => FilePath -> m TL.Text
readSampleIO fpath =
  liftIO $
    TL.readFile $
      "test/resources" </> fpath

authrspTestReqId :: SAML.ID SAML.AuthnRequest
authrspTestReqId = SAML.ID "authrsp-test-req-id"

authrspTestTeamId :: TeamId
authrspTestTeamId = either error id $ parseIdFromText "aaaabbbb-cccc-dddd-eeee-ffffffffffff"

type AuthrespEffs =
  '[ SAML2,
     SamlProtocolSettings,
     AssIDStore,
     VerdictFormatStore.VerdictFormatStore,
     AReqIDStore,
     Logger String,
     Reporter,
     Input Spar.Options.Opts,
     Random,
     SAMLUserStore,
     GalleyAPIAccess,
     BrigAPIAccess,
     ScimTokenStore,
     IdPConfigStore,
     IdPRawMetadataStore,
     Logger (Msg -> Msg),
     Polysemy.Error.Error SparError,
     Embed IO,
     Final IO
   ]

saml2Mock ::
  forall r a.
  (NonEmptyL.NonEmpty SAML.Assertion, IdP, SAML.AccessVerdict) ->
  Sem (SAML2 ': r) a ->
  Sem r a
saml2Mock triplet@(assertions, idp, verdict) = interpretH $ \case
  AuthResp _ _ _ continue _ -> do
    next <- runT (continue assertions idp verdict)
    raise (saml2Mock triplet next)
  _ -> error "saml2Mock: unexpected constructor"

verdictFormatMock :: Sem (VerdictFormatStore.VerdictFormatStore ': r) a -> Sem r a
verdictFormatMock = interpret $ \case
  VerdictFormatStore.Get _ -> pure (Just (VerdictFormatWeb Nothing))
  VerdictFormatStore.Store {} -> pure ()

noopAReqIDStoreMock :: Sem (AReqIDStore ': r) a -> Sem r a
noopAReqIDStoreMock = interpret $ \_ -> error "AReqIDStore called unexpectedly in authresp test"

noopAssIDStoreMock :: Sem (AssIDStore ': r) a -> Sem r a
noopAssIDStoreMock = interpret $ \_ -> error "AssIDStore called unexpectedly in authresp test"

noopReporter :: Sem (Reporter ': r) a -> Sem r a
noopReporter = interpret $ \(Report _ _) -> pure ()

interpretAuthrespE ::
  Spar.Options.Opts ->
  Maybe User ->
  (NonEmptyL.NonEmpty SAML.Assertion, IdP, SAML.AccessVerdict) ->
  Sem AuthrespEffs a ->
  IO ([LogLine], Either SparError a)
interpretAuthrespE opts mbAccount triplet action = do
  lr <- newLogRecorder
  a <-
    runFinal
      . embedToFinal @IO
      . Polysemy.Error.errorToIOFinal
      . recordLogs lr
      . ignoringState idpRawMetadataStoreToMem
      . ignoringState idPToMem
      . ignoringState scimTokenStoreToMem
      . brigAccessMock mbAccount
      . galleyAccessMock
      . ignoringState samlUserStoreToMem
      . randomToNull
      . runInputConst opts
      . noopReporter
      . discardLogs
      . noopAReqIDStoreMock
      . verdictFormatMock
      . noopAssIDStoreMock
      . sparRouteToServant (Spar.Options.saml opts)
      . saml2Mock triplet
      $ action
  logs <- readIORef lr.recordedLogs
  pure $ case a of
    Left e -> (logs, Left e)
    Right (_, res) -> (logs, Right res)
