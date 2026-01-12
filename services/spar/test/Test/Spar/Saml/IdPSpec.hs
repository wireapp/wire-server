module Test.Spar.Saml.IdPSpec where

import Arbitrary ()
import Control.Lens ((.~), (^.))
import Data.Domain
import Data.Id (parseIdFromText)
import qualified Data.List.NonEmpty as NonEmptyL
import qualified Data.Map as Map
import Data.Range
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Imports
import Polysemy
import qualified Polysemy.Error
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
import System.FilePath
import System.Logger (Msg)
import System.Logger.Class (Level (..))
import Test.Hspec
import Test.QuickCheck
import qualified Text.XML.DSig as DSig
import URI.ByteString (parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri)
import Wire.API.User (User (..))
import Wire.API.User.IdentityProvider (IdPMetadataInfo (..), WireIdPAPIVersion (..), idpMetadataRecord)
import Wire.Sem.Logger.TinyLog (LogRecorder (..), newLogRecorder, recordLogs)
import Wire.Sem.Random
import Wire.Sem.Random.Null

spec :: Spec
spec = describe "SAML IdP change logging" $ do
  describe "idp-create" $ do
    it "should log IdP creation" $ do
      idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
      let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
          zUser = Just <$> either error id $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
          samlConfig =
            Config
              { -- The log level only matters for log output, not production.
                -- Thus, we could put anything here, it just needs to be a valid
                -- value.
                _cfgLogLevel = Trace,
                _cfgSPHost = "localhost",
                _cfgSPPort = 8081,
                _cfgDomainConfigs =
                  Left
                    MultiIngressDomainConfig
                      { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                        _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                        _cfgContacts = [fallbackContact]
                      }
              }
          host = Just "backend.example.com"
          idpHandle = Just $ unsafeRange "some-idp"
          apiVersion = Just WireIdPAPIV2
          issuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://accounts.accesscontrol.windows.net/auth"
          idPMetadataInfo' = idPMetadataInfo & idpMetadataRecord . SAML.edIssuer .~ issuer
          expectedLogLine =
            ( Info,
              "IdP created, team=6861026d-cdee-3da5-22fc-6612bb1360b8, idpId=00000000-0000-0000-0000-000000000000, issuer=https://accounts.accesscontrol.windows.net/auth, domain=None, user=59128ccc-d38a-1d23-67d9-4f529ee7ca9f, certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37, replaces=None\n"
            )

      (logs, _res) <- interpretWithLoggingMock Nothing (idpCreate samlConfig tid zUser host idPMetadataInfo' Nothing apiVersion idpHandle)
      logs `shouldContain` [expectedLogLine]

      (logsV7, _res) <- interpretWithLoggingMock Nothing (idpCreateV7 samlConfig tid zUser idPMetadataInfo' Nothing apiVersion idpHandle)
      logsV7 `shouldContain` [expectedLogLine]

    it "should log IdP creation with domain for multi-ingress" $ do
      idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
      let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
          zUser = Just <$> either error id $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
          samlConfig =
            Config
              { -- The log level only matters for log output, not production.
                -- Thus, we could put anything here, it just needs to be a valid
                -- value.
                _cfgLogLevel = Trace,
                _cfgSPHost = "localhost",
                _cfgSPPort = 8081,
                _cfgDomainConfigs =
                  Right $
                    Map.fromList
                      [ ( miDomain,
                          MultiIngressDomainConfig
                            { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                              _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                              _cfgContacts = [fallbackContact]
                            }
                        )
                      ]
              }
          domainAsText = "backend.example.com"
          miDomain = either (error . show) id $ mkDomain domainAsText
          host = Just domainAsText
          idpHandle = Just $ unsafeRange "some-idp"
          apiVersion = Just WireIdPAPIV2
          issuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://accounts.accesscontrol.windows.net/auth"
          idPMetadataInfo' = idPMetadataInfo & idpMetadataRecord . SAML.edIssuer .~ issuer
          expectedLogLine :: LByteString -> LogLine
          expectedLogLine domainPart =
            ( Info,
              "IdP created, team=6861026d-cdee-3da5-22fc-6612bb1360b8, idpId=00000000-0000-0000-0000-000000000000, issuer=https://accounts.accesscontrol.windows.net/auth, domain=" <> domainPart <> ", user=59128ccc-d38a-1d23-67d9-4f529ee7ca9f, certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37, replaces=None\n"
            )
          expectedLogLineWithDomain = expectedLogLine . TL.encodeUtf8 . TL.fromStrict $ domainAsText
          expectedLogLineWithoutDomain = expectedLogLine "None"

      (logs, _res) <- interpretWithLoggingMock Nothing (idpCreate samlConfig tid zUser host idPMetadataInfo' Nothing apiVersion idpHandle)
      logs `shouldContain` [expectedLogLineWithDomain]

      -- >=V7 does not bother with multi-ingress domains for IdPs as it can
      -- only have one IdP per team anyways.
      (logsV7, _res) <- interpretWithLoggingMock Nothing (idpCreateV7 samlConfig tid zUser idPMetadataInfo' Nothing apiVersion idpHandle)
      logsV7 `shouldContain` [expectedLogLineWithoutDomain]
  describe "idp-delete" $ do
    it "should log IdP deletion" $ do
      idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
      user :: User <- generate arbitrary
      let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
          zUser = Just <$> either error id $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
          host = Just "backend.example.com"
          idpHandle = Just $ unsafeRange "some-idp"
          apiVersion = Just WireIdPAPIV2
          issuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://accounts.accesscontrol.windows.net/auth"
          idPMetadataInfo' = idPMetadataInfo & idpMetadataRecord . SAML.edIssuer .~ issuer
          samlConfig =
            Config
              { -- The log level only matters for log output, not production.
                -- Thus, we could put anything here, it just needs to be a valid
                -- value.
                _cfgLogLevel = Trace,
                _cfgSPHost = "localhost",
                _cfgSPPort = 8081,
                _cfgDomainConfigs =
                  Left
                    MultiIngressDomainConfig
                      { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                        _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                        _cfgContacts = [fallbackContact]
                      }
              }
          expectedLogLine =
            ( Info,
              "IdP deleted, team=6861026d-cdee-3da5-22fc-6612bb1360b8, idpId=00000000-0000-0000-0000-000000000000, issuer=https://accounts.accesscontrol.windows.net/auth, domain=None, user=59128ccc-d38a-1d23-67d9-4f529ee7ca9f, certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37\n"
            )

      (logs, _res) <- interpretWithLoggingMock (Just user) $ do
        idp <- idpCreate samlConfig tid zUser host idPMetadataInfo' Nothing apiVersion idpHandle
        idpDelete zUser (idp ^. idpId) Nothing
      logs `shouldContain` [expectedLogLine]

  describe "idp-update" $ do
    it "should log IdP update" $ do
      idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
      user :: User <- generate arbitrary
      let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
          zUser = Just <$> either error id $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
          host = Just "backend.example.com"
          idpHandle = Just $ unsafeRange "some-idp"
          apiVersion = Just WireIdPAPIV2
          issuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://accounts.accesscontrol.windows.net/auth"
          idPMetadataInfo' = idPMetadataInfo & idpMetadataRecord . SAML.edIssuer .~ issuer
          samlConfig =
            Config
              { -- The log level only matters for log output, not production.
                -- Thus, we could put anything here, it just needs to be a valid
                -- value.
                _cfgLogLevel = Trace,
                _cfgSPHost = "localhost",
                _cfgSPPort = 8081,
                _cfgDomainConfigs =
                  Left
                    MultiIngressDomainConfig
                      { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                        _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                        _cfgContacts = [fallbackContact]
                      }
              }
          expectedLogLine = (Info, "IdP updated, team=6861026d-cdee-3da5-22fc-6612bb1360b8, idpId=00000000-0000-0000-0000-000000000000, issuer=https://accounts.accesscontrol.windows.net/auth, domain=None, user=59128ccc-d38a-1d23-67d9-4f529ee7ca9f, new-certificates=, removed-certificates=\n")

      (logs, _res) <- interpretWithLoggingMock (Just user) $ do
        idp <- idpCreate samlConfig tid zUser host idPMetadataInfo' Nothing apiVersion idpHandle
        idpUpdate samlConfig zUser host idPMetadataInfo' (idp ^. idpId) Nothing
      logs `shouldContain` [expectedLogLine]

    it "should log IdP update (changed cert)" $ do
      idPMetadataInfo :: IdPMetadataInfo <- generate arbitrary
      user :: User <- generate arbitrary
      newKeyInfo <- readSampleIO "okta-keyinfo-1.xml"
      let tid = either error id $ parseIdFromText "6861026d-cdee-3da5-22fc-6612bb1360b8"
          zUser = Just <$> either error id $ parseIdFromText "59128ccc-d38a-1d23-67d9-4f529ee7ca9f"
          host = Just "backend.example.com"
          idpHandle = Just $ unsafeRange "some-idp"
          apiVersion = Just WireIdPAPIV2
          issuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://accounts.accesscontrol.windows.net/auth"
          newIssuer = Issuer . (either (error . show) id) . parseURI strictURIParserOptions . fromString $ "https://new.idp.example.com/auth"
          newRequestURI = either (error . show) id . parseURI strictURIParserOptions . fromString $ "https://new.idp.example.com/login"
          idPMetadataInfo' = idPMetadataInfo & idpMetadataRecord . SAML.edIssuer .~ issuer
          newCert = either (error . show) id $ DSig.parseKeyInfo False newKeyInfo
          newIdPMetadata :: IdPMetadata =
            IdPMetadata
              { _edIssuer = newIssuer,
                _edRequestURI = newRequestURI,
                _edCertAuthnResponse = NonEmptyL.singleton newCert
              }
          idPMetadataInfo'' = IdPMetadataValue ((TL.toStrict . encode) newIdPMetadata) newIdPMetadata
          samlConfig =
            Config
              { -- The log level only matters for log output, not production.
                -- Thus, we could put anything here, it just needs to be a valid
                -- value.
                _cfgLogLevel = Trace,
                _cfgSPHost = "localhost",
                _cfgSPPort = 8081,
                _cfgDomainConfigs =
                  Left
                    MultiIngressDomainConfig
                      { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
                        _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
                        _cfgContacts = [fallbackContact]
                      }
              }
          expectedLogLine =
            ( Info,
              "IdP updated, team=6861026d-cdee-3da5-22fc-6612bb1360b8, idpId=00000000-0000-0000-0000-000000000000, issuer=https://new.idp.example.com/auth, domain=None, user=59128ccc-d38a-1d23-67d9-4f529ee7ca9f, new-certificates=Issuer: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; Subject: Country=US,O=Okta,OU=SSOProvider,CN=dev-500508,Email Address=info@okta.com; SHA1 Fingerprint: 5C:42:5B:27:B3:96:CC:9D:1B:1F:0E:4F:2B:8A:B8:E4:3C:9E:96:34, removed-certificates=Issuer: CN=accounts.accesscontrol.windows.net; Subject: CN=accounts.accesscontrol.windows.net; SHA1 Fingerprint: 15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37\n"
            )

      (logs, _res) <- interpretWithLoggingMock (Just user) $ do
        idp <- idpCreate samlConfig tid zUser host idPMetadataInfo' Nothing apiVersion idpHandle
        idpUpdate samlConfig zUser host idPMetadataInfo'' (idp ^. idpId) Nothing
      logs `shouldContain` [expectedLogLine]

type LogLine = (Level, LByteString)

interpretWithLoggingMock ::
  Maybe User ->
  Sem (Effs) a ->
  IO ([LogLine], a)
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
  -- TODO: Better error handling
  pure (logs, either (error . show) id a)

-- TODO: Is this general enough to extract it and provide it for other tests?
galleyAccessMock :: Sem (GalleyAccess ': r) a -> Sem r a
galleyAccessMock = interpret $ \case
  GetTeamMembers _teamId -> undefined
  GetTeamMember _teamId _userId -> undefined
  AssertHasPermission _teamId _perm _userId -> pure ()
  AssertSSOEnabled _teamId -> pure ()
  IsEmailValidationEnabledTeam _teamId -> undefined
  UpdateTeamMember _userId _teamId _role -> undefined

-- TODO: Is this general enough to extract it and provide it for other tests?
brigAccessMock :: Maybe User -> Sem (BrigAccess ': r) a -> Sem r a
brigAccessMock mbAccount = interpret $ \case
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
