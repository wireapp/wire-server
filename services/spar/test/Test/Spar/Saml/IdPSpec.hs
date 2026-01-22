module Test.Spar.Saml.IdPSpec where

import Arbitrary ()
import Data.Domain
import Data.Id (idToText, parseIdFromText)
import qualified Data.List.NonEmpty as NonEmptyL
import qualified Data.Map as Map
import Data.Range
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Imports
import Polysemy
import qualified Polysemy.Error
import Polysemy.State
import Polysemy.TinyLog
import SAML2.WebSSO
import qualified SAML2.WebSSO as SAML
import Spar.API (idpCreate, idpCreateV7, idpDelete, idpUpdate)
import Spar.Error
import Spar.Sem.BrigAccess
import Spar.Sem.GalleyAccess
import Spar.Sem.IdPConfigStore
import Spar.Sem.IdPConfigStore.Mem
import Spar.Sem.IdPRawMetadataStore
import Spar.Sem.IdPRawMetadataStore.Mem
import Spar.Sem.SAMLUserStore
import Spar.Sem.SAMLUserStore.Mem
import Spar.Sem.ScimTokenStore
import Spar.Sem.ScimTokenStore.Mem
import System.FilePath ((</>))
import System.Logger (Msg)
import System.Logger.Class (Level (..))
import Test.Hspec
import Test.QuickCheck
import qualified Text.XML.DSig as DSig
import URI.ByteString (parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri)
import Wire.API.Routes.Internal.Brig (IdpChangedNotification (..))
import Wire.API.User (User (..))
import Wire.API.User.IdentityProvider (IdPMetadataInfo (..), WireIdPAPIVersion (..))
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

                  -- >=V7 does not bother with multi-ingress domains for IdPs as it can
                  -- only have one IdP per team anyways.
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

                  -- >=V7 does not bother with multi-ingress domains for IdPs as it can
                  -- only have one IdP per team anyways.
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
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary

                forM_ [(minBound :: WireIdPAPIVersion) .. maxBound] $ \apiVersion -> do
                  (_logs, notifs, _idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifs `shouldBe` mempty

                  -- >=V7 does not bother with multi-ingress domains for IdPs as it can
                  -- only have one IdP per team anyways.
                  (_logs, notifsV7, _idp) <-
                    interpretWithLoggingMock
                      Nothing
                      (idpCreateV7 singleIngressSamlConfig tid zUser idPMetadataInfo Nothing (Just apiVersion) idpHandle)
                  notifsV7 `shouldBe` mempty
            describe "idp-delete" $ do
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, _) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  idpDelete singleIngressSamlConfig zUser (idp._idpId) Nothing
                notifs `shouldBe` mempty
            describe "idp-update" $ do
              it "should send" $ do
                idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
                user :: User <- generate arbitrary

                (_logs, notifs, _) <- interpretWithLoggingMock (Just user) $ do
                  idp <- idpCreate singleIngressSamlConfig tid zUser miHost1 idPMetadataInfo Nothing apiVersionV2 idpHandle
                  idpUpdate singleIngressSamlConfig zUser miHost1 idPMetadataInfo (idp._idpId) Nothing
                notifs `shouldBe` mempty

type LogLine = (Level, LByteString)

interpretWithLoggingMock ::
  Maybe User ->
  Sem (Effs) a ->
  IO ([LogLine], [IdpChangedNotification], a)
interpretWithLoggingMock mbAccount action = do
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
      $ action
  logs <- readIORef lr.recordedLogs
  let (notifs, res) = either (error . show) id a
  pure (logs, notifs, res)

galleyAccessMock :: Sem (GalleyAccess ': r) a -> Sem r a
galleyAccessMock = interpret $ \case
  GetTeamMembers _teamId -> undefined
  GetTeamMember _teamId _userId -> undefined
  AssertHasPermission _teamId _perm _userId -> pure ()
  AssertSSOEnabled _teamId -> pure ()
  IsEmailValidationEnabledTeam _teamId -> undefined
  UpdateTeamMember _userId _teamId _role -> undefined

brigAccessMock :: Maybe User -> Sem (BrigAccess ': r) a -> Sem r ([IdpChangedNotification], a)
brigAccessMock mbAccount = (runState @([IdpChangedNotification]) mempty .) $
  reinterpret $ \case
    CreateSAML _userRef _userId _teamId _name _managedBy _mHandle _mRichInfo _mLocale _role -> undefined
    CreateNoSAML _txt _email _userId _teamId _name _mLocale _role -> undefined
    UpdateEmail _userId _email _activation -> undefined
    GetAccount _havePendingInvitations _userId -> pure mbAccount
    GetByHandle _handle -> undefined
    GetByEmail _email -> undefined
    SetName _userId _name -> undefined
    SetHandle _userId _handle -> undefined
    SetManagedBy _userId _managedBy -> undefined
    SetSSOId _userId _ssoId -> undefined
    SetRichInfo _userId _richInfo -> undefined
    SetLocale _userId _mLocale -> undefined
    GetRichInfo _userId -> undefined
    CheckHandleAvailable _handle -> undefined
    DeleteUser _userId -> undefined
    EnsureReAuthorised _mUserId _mPassword _mCode _mAction -> undefined
    SsoLogin _userId -> undefined
    GetStatus _userId -> undefined
    GetStatusMaybe _userId -> undefined
    SetStatus _userId _status -> undefined
    GetDefaultUserLocale -> undefined
    CheckAdminGetTeamId _userId -> undefined
    SendSAMLIdPChangedEmail notif -> modify (notif :)

ignoringState :: (Functor f) => (a -> f (c, b)) -> a -> f b
ignoringState f = fmap snd . f

type Effs =
  '[ Random,
     SAMLUserStore,
     GalleyAccess,
     BrigAccess,
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
