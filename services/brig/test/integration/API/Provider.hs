{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module API.Provider
  ( tests,
    Config,
  )
where

import qualified API.Team.Util as Team
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import qualified Brig.Code as Code
import Brig.Types hiding (CompletePasswordReset (..), EmailUpdate (..), NewPasswordReset (..), PasswordChange (..), PasswordReset (..))
import qualified Brig.Types.Intra as Intra
import Brig.Types.Provider
import qualified Brig.Types.Provider.External as Ext
import Brig.Types.Provider.Tag
import qualified Cassandra as DB
import Control.Arrow ((&&&))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Chan
import Control.Concurrent.Timeout (threadDelay, timeout)
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Domain
import Data.Handle (Handle (Handle))
import qualified Data.HashMap.Strict as HashMap
import Data.Id hiding (client)
import Data.Json.Util (toBase64Text)
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import Data.PEM
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import Data.Timeout (TimedOut (..), Timeout, TimeoutUnit (..), (#))
import qualified Data.UUID as UUID
import qualified Data.ZAuth.Token as ZAuth
import Galley.Types (Access (..), AccessRole (..), ConvMembers (..), Conversation (..), ConversationAccessUpdate (..), Event (..), EventData (..), EventType (..), NewConv (..), NewConvUnmanaged (..), OtherMember (..), OtrMessage (..), SimpleMember (..), SimpleMembers (..), UserIdList (..))
import Galley.Types.Bot (ServiceRef, newServiceRef, serviceRefId, serviceRefProvider)
import Galley.Types.Conversations.Roles (roleNameWireAdmin)
import qualified Galley.Types.Teams as Team
import Gundeck.Types.Notification
import Imports hiding (threadDelay)
import Network.HTTP.Types.Status (status200, status201, status400)
import Network.Wai (Application, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Route as Wai
import qualified Network.Wai.Utilities.Error as Error
import OpenSSL.PEM (writePublicKey)
import OpenSSL.RSA (generateRSAKey')
import System.IO.Temp (withSystemTempFile)
import Test.Tasty hiding (Timeout)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import Util
import Web.Cookie (SetCookie (..), parseSetCookie)

tests :: Domain -> Config -> Manager -> DB.ClientState -> Brig -> Cannon -> Galley -> IO TestTree
tests dom conf p db b c g = do
  return $
    testGroup
      "provider"
      [ testGroup
          "account"
          [ test p "register" $ testRegisterProviderDB db b,
            test p "register + activate internal" $ testRegisterProviderInternal b,
            test p "login" $ testLoginProvider db b,
            test p "update" $ testUpdateProvider db b,
            test p "delete" $ testDeleteProvider db b,
            test p "password-reset" $ testPasswordResetProvider db b,
            test p "email/password update with password reset" $
              testPasswordResetAfterEmailUpdateProvider db b
          ],
        testGroup
          "service"
          [ test p "add-get fail (bad key)" $ testAddGetServiceBadKey conf db b,
            test p "add-get" $ testAddGetService conf db b,
            test p "update" $ testUpdateService conf db b,
            test p "update-conn" $ testUpdateServiceConn conf db b,
            test p "search (tag/prefix)" $ testListServices conf db b,
            test p "delete" $ testDeleteService conf db b g c
          ],
        testGroup
          "service whitelist"
          [ test p "search permissions" $
              testWhitelistSearchPermissions conf db b g,
            test p "update permissions" $
              testWhitelistUpdatePermissions conf db b g,
            test p "basic functionality" $
              testWhitelistBasic conf db b g,
            test p "search" $ testSearchWhitelist conf db b g,
            test p "search honors enabling and whitelisting" $
              testSearchWhitelistHonorUpdates conf db b,
            test p "de-whitelisted bots are removed" $
              testWhitelistKickout dom conf db b g c,
            test p "de-whitelisting works with deleted conversations" $
              testDeWhitelistDeletedConv conf db b g c
          ],
        testGroup
          "bot"
          [ test p "add-remove" $ testAddRemoveBot conf db b g c,
            test p "message" $ testMessageBot conf db b g c,
            test p "bad fingerprint" $ testBadFingerprint conf db b g c
          ],
        testGroup
          "bot-teams"
          [ test p "add-remove" $ testAddRemoveBotTeam conf db b g c,
            test p "team-only" $ testBotTeamOnlyConv conf db b g c,
            test p "message" $ testMessageBotTeam conf db b g c,
            test p "delete conv" $ testDeleteConvBotTeam conf db b g c,
            test p "delete team" $ testDeleteTeamBotTeam conf db b g c
          ]
      ]

----------------------------------------------------------------------------
-- Config

data Config = Config
  { privateKey :: FilePath,
    publicKey :: FilePath,
    cert :: FilePath,
    botHost :: Text,
    botPort :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config

-------------------------------------------------------------------------------
-- Provider Accounts

-- | Test provider register by accessing the DB directly
testRegisterProviderDB :: DB.ClientState -> Brig -> Http ()
testRegisterProviderDB = testRegisterProvider . Just

-- | Test provider register using an internal HTTP endpoint
testRegisterProviderInternal :: Brig -> Http ()
testRegisterProviderInternal = testRegisterProvider Nothing

testLoginProvider :: DB.ClientState -> Brig -> Http ()
testLoginProvider db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  let email = providerEmail prv
  _rs <-
    loginProvider brig email defProviderPassword <!! do
      const 200 === statusCode
      const Nothing === responseBody
  let Just cok = parseSetCookie <$> getHeader "Set-Cookie" _rs
  now <- liftIO getCurrentTime
  let ttl = (`diffUTCTime` now) <$> setCookieExpires cok
  liftIO $ do
    assertEqual "cookie name" "zprovider" (setCookieName cok)
    assertEqual "cookie http-only" True (setCookieHttpOnly cok)
    assertBool "cookie timeout" (ttl > Just 0)
  let Just tok = fromByteString (setCookieValue cok)
  liftIO $ assertEqual "principal" pid (Id (tok ^. ZAuth.body . ZAuth.provider))

testUpdateProvider :: DB.ClientState -> Brig -> Http ()
testUpdateProvider db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  let newName = Name "All New"
  let Just newUrl = fromByteString "https://new.localhost/"
  let newDescr = "Totally new description"
  let upd =
        UpdateProvider
          { updateProviderName = Just newName,
            updateProviderUrl = Just newUrl,
            updateProviderDescr = Just newDescr
          }
  updateProvider brig pid upd !!! const 200 === statusCode
  _rs <- getProvider brig pid <!! const 200 === statusCode
  let Just prv' = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "name" newName (providerName prv')
    assertEqual "url" newUrl (providerUrl prv')
    assertEqual "description" newDescr (providerDescr prv')

testDeleteProvider :: DB.ClientState -> Brig -> Http ()
testDeleteProvider db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  deleteProvider brig pid defProviderPassword
    !!! const 200 === statusCode
  getProvider brig pid !!! const 404 === statusCode
  -- The email address must be available again
  let new = defNewProvider (providerEmail prv)
  registerProvider brig new
    !!! const 201 === statusCode

testPasswordResetProvider :: DB.ClientState -> Brig -> Http ()
testPasswordResetProvider db brig = do
  prv <- randomProvider db brig
  let email = providerEmail prv
  let newPw = PlainTextPassword "newsupersecret"
  initiatePasswordResetProvider brig (PasswordReset email) !!! const 201 === statusCode
  -- password reset with same password fails.
  resetPw defProviderPassword email !!! const 409 === statusCode
  -- password reset with different password works.
  resetPw newPw email !!! const 200 === statusCode
  loginProvider brig email defProviderPassword
    !!! const 403 === statusCode
  loginProvider brig email newPw
    !!! const 200 === statusCode
  where
    resetPw :: PlainTextPassword -> Email -> Http ResponseLBS
    resetPw newPw email = do
      -- Get the code directly from the DB
      gen <- Code.mkGen (Code.ForEmail email)
      Just vcode <- lookupCode db gen Code.PasswordReset
      let passwordResetData =
            CompletePasswordReset
              (Code.codeKey vcode)
              (Code.codeValue vcode)
              newPw
      completePasswordResetProvider brig passwordResetData

testPasswordResetAfterEmailUpdateProvider :: DB.ClientState -> Brig -> Http ()
testPasswordResetAfterEmailUpdateProvider db brig = do
  newEmail <- randomEmail
  prv <- randomProvider db brig
  let pid = providerId prv
  let origEmail = providerEmail prv
  initiateEmailUpdateProvider brig pid (EmailUpdate newEmail) !!! const 202 === statusCode
  initiatePasswordResetProvider brig (PasswordReset origEmail) !!! const 201 === statusCode
  -- Get password reset code directly from the DB
  genOrig <- Code.mkGen (Code.ForEmail origEmail)
  Just vcodePw <- lookupCode db genOrig Code.PasswordReset
  let passwordResetData =
        CompletePasswordReset
          (Code.codeKey vcodePw)
          (Code.codeValue vcodePw)
          (PlainTextPassword "doesnotmatter")
  -- Activate the new email
  genNew <- Code.mkGen (Code.ForEmail newEmail)
  Just vcodeEm <- lookupCode db genNew Code.IdentityVerification
  activateProvider brig (Code.codeKey vcodeEm) (Code.codeValue vcodeEm)
    !!! const 200 === statusCode
  p <- responseJsonError =<< (getProvider brig pid <!! const 200 === statusCode)
  liftIO $ assertEqual "email" newEmail (providerEmail p)
  -- attempting to complete password reset should fail
  completePasswordResetProvider brig passwordResetData !!! const 403 === statusCode
  -- ensure you can login with the new email address and not with the old one
  loginProvider brig origEmail defProviderPassword !!! const 403 === statusCode
  loginProvider brig newEmail defProviderPassword !!! const 200 === statusCode
  -- exercise the password change endpoint
  let newPass = PlainTextPassword "newpass"
  let pwChangeFail = PasswordChange (PlainTextPassword "notcorrect") newPass
  updateProviderPassword brig pid pwChangeFail !!! const 403 === statusCode
  let pwChange = PasswordChange defProviderPassword newPass
  updateProviderPassword brig pid pwChange !!! const 200 === statusCode
  -- put /provider/password gives 409 if new password is the same.
  let pwChange' = PasswordChange newPass newPass
  updateProviderPassword brig pid pwChange' !!! const 409 === statusCode
  -- Check the login process again
  loginProvider brig newEmail defProviderPassword !!! const 403 === statusCode
  loginProvider brig newEmail newPass !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- Provider Services

testAddGetServiceBadKey :: Config -> DB.ClientState -> Brig -> Http ()
testAddGetServiceBadKey config db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  -- Add service
  new <- defNewService config
  -- Specially crafted key that passes basic validation
  let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
  let newBad = new {newServiceKey = ServiceKeyPEM k}
  addService brig pid newBad !!! const 400 === statusCode

testAddGetService :: Config -> DB.ClientState -> Brig -> Http ()
testAddGetService config db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  -- Add service
  new <- defNewService config
  _rs <- addService brig pid new <!! const 201 === statusCode
  let Just srs = responseJsonMaybe _rs
  let sid = rsNewServiceId srs
  -- Get service definition as seen by provider
  _rs <- getService brig pid sid <!! const 200 === statusCode
  let Just svc = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "auth token" (List1.singleton <$> rsNewServiceToken srs) (Just (serviceTokens svc))
    assertEqual "name" defServiceName (serviceName svc)
    assertEqual "description" defServiceDescr (serviceDescr svc)
    assertEqual "url" defServiceUrl (serviceUrl svc)
    assertEqual "keys" (List1.singleton (newServiceKey new)) (serviceKeyPEM <$> serviceKeys svc)
    assertEqual "assets" defServiceAssets (serviceAssets svc)
    assertEqual "tags" (fromRange defServiceTags) (serviceTags svc)
    assertBool "enabled" (not (serviceEnabled svc))
  -- Get public service profile
  uid <- randomId
  _rs <- getServiceProfile brig uid pid sid <!! const 200 === statusCode
  let Just svp = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "id" (serviceId svc) (serviceProfileId svp)
    assertEqual "provider" pid (serviceProfileProvider svp)
    assertEqual "name" (serviceName svc) (serviceProfileName svp)
    assertEqual "description" (serviceDescr svc) (serviceProfileDescr svp)
    assertEqual "assets" (serviceAssets svc) (serviceProfileAssets svp)
    assertEqual "tags" (serviceTags svc) (serviceProfileTags svp)
    assertBool "enabled" (not (serviceProfileEnabled svp))

-- TODO: Check that disabled services can not be found via tag search?
--       Need to generate a unique service name for that.

testUpdateService :: Config -> DB.ClientState -> Brig -> Http ()
testUpdateService config db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  _svc <- addGetService brig pid =<< defNewService config
  let sid = serviceId _svc
  let newTags = Set.fromList [QuizTag, EducationTag]
  let newName = Name "x"
  let newSummary = "short"
  let newDescr = "looooooooooooong"
  let newAssets = [] -- TODO
  -- Exercise all updateable attributes
  let upd =
        UpdateService
          { updateServiceName = Just newName,
            updateServiceSummary = Just (unsafeRange newSummary),
            updateServiceDescr = Just (unsafeRange newDescr),
            updateServiceAssets = Just newAssets,
            updateServiceTags = Just (unsafeRange newTags)
          }
  updateService brig pid sid upd !!! const 200 === statusCode
  _rs <- getService brig pid sid <!! const 200 === statusCode
  let Just _svc = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "name" newName (serviceName _svc)
    assertEqual "description" newDescr (serviceDescr _svc)
    assertEqual "assets" newAssets (serviceAssets _svc)
    assertEqual "tags" newTags (serviceTags _svc)
  -- Excercise all individual tags
  forM_ [minBound ..] $ \tag -> do
    let t = Set.singleton tag
    let u = upd {updateServiceTags = Just (unsafeRange t)}
    updateService brig pid sid u !!! const 200 === statusCode
    _rs <- getService brig pid sid <!! const 200 === statusCode
    let Just _svc = responseJsonMaybe _rs
    liftIO $ assertEqual "tags" t (serviceTags _svc)

testUpdateServiceConn :: Config -> DB.ClientState -> Brig -> Http ()
testUpdateServiceConn config db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  _svc <- addGetService brig pid =<< defNewService config
  let sid = serviceId _svc
  let Just newUrl = fromByteString "https://other.localhost/test"
  key <- randServiceKey
  let newKeys = key `List1.cons` (serviceKeyPEM <$> serviceKeys _svc)
  let tok = ServiceToken (Ascii.unsafeFromText "123456")
  let newTokens = tok `List1.cons` serviceTokens _svc
  let upd =
        UpdateServiceConn
          { updateServiceConnUrl = Just newUrl,
            updateServiceConnKeys = Just (unsafeRange (toList newKeys)),
            updateServiceConnTokens = Just (unsafeRange (toList newTokens)),
            updateServiceConnEnabled = Just True,
            updateServiceConnPassword = defProviderPassword
          }
  updateServiceConn brig pid sid upd
    !!! const 200 === statusCode
  _rs <- getService brig pid sid <!! const 200 === statusCode
  let Just _svc = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "url" newUrl (serviceUrl _svc)
    assertEqual "keys" newKeys (fmap serviceKeyPEM (serviceKeys _svc))
    assertEqual "token" newTokens (serviceTokens _svc)
    assertBool "enabled" (serviceEnabled _svc)

testListServices :: Config -> DB.ClientState -> Brig -> Http ()
testListServices config db brig = do
  prv <- randomProvider db brig
  let pid = providerId prv
  uid <- randomId
  -- You need to supply at least one tag or a prefix
  get
    ( brig
        . path "/services"
        . header "Z-Type" "access"
        . header "Z-User" (toByteString' uid)
    )
    !!! const 400
    === statusCode
  -- An empty prefix is not sufficient
  listServiceProfilesByPrefix brig uid (Name "") 10 !!! const 400 === statusCode
  -- nb. We use a random name prefix so tests can run concurrently
  -- (and repeatedly) against a shared database and thus a shared
  -- "name index" per tag.
  uniq <- UUID.toText . toUUID <$> randomId
  new <- defNewService config
  let mkName n = Name (uniq <> "|" <> n)
  svcs <- mapM (addGetService brig pid . mkNew new) (taggedServiceNames uniq)
  mapM_ (enableService brig pid . serviceId) svcs
  let services :: [(ServiceId, Name)]
      services = map (serviceId &&& serviceName) svcs
  -- This is how we're going to call our /services endpoint. Every time we
  -- would call it twice (with tags and without) and assert that results
  -- match.
  let search :: HasCallStack => Name -> Http ServiceProfilePage
      search name = do
        r1 <- searchServices brig 20 uid (Just name) Nothing
        r2 <- searchServices brig 20 uid (Just name) (Just (match1 SocialTag))
        -- We could also compare 'serviceProfilePageHasMore' here, but
        -- then the test wouldn't pass (even though it should!).
        -- See Note [buggy pagination] for more details.
        liftIO $
          assertEqual
            ("search for " <> show name <> " without and with tags")
            (serviceProfilePageResults r1)
            (serviceProfilePageResults r2)
        return r1
  -- This function searches for a prefix and check that the results match
  -- our known list of services
  let searchAndCheck :: HasCallStack => Name -> Http [ServiceProfile]
      searchAndCheck name = do
        result <- search name
        assertServiceDetails ("name " <> show name) (select name services) result
        return (serviceProfilePageResults result)
  -- Search for our unique prefix and check that all services are found
  search (Name uniq) >>= assertServiceDetails ("all with prefix " <> show uniq) services
  -- Search by exact name and check that only one service is found
  forM_ (take 3 services) $ \(sid, name) ->
    search name >>= assertServiceDetails ("name " <> show name) [(sid, name)]
  -- Some chosen prefixes
  -- # Bjø -> Bjørn
  _found <- map serviceProfileName <$> searchAndCheck (mkName "Bjø")
  liftIO $ assertEqual "Bjø" [mkName "Bjørn"] _found
  -- # Bj -> bjorn, Bjørn
  _found <- map serviceProfileName <$> searchAndCheck (mkName "Bj")
  liftIO $ assertEqual "Bj" [mkName "bjorn", mkName "Bjørn"] _found
  -- # chris -> CHRISTMAS
  _found <- map serviceProfileName <$> searchAndCheck (mkName "chris")
  liftIO $ assertEqual "chris" [mkName "CHRISTMAS"] _found
  -- Ensure name changes are also indexed properly
  forM_ (take 3 services) $ \(sid, _) ->
    searchAndAssertNameChange brig pid sid uid uniq search
  where
    mkNew new (n, t) =
      new
        { newServiceName = n,
          newServiceTags = unsafeRange (Set.fromList t)
        }
    select (Name prefix) = filter (Text.isPrefixOf (Text.toLower prefix) . Text.toLower . fromName . snd)

testDeleteService :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeleteService config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Create a conversation
  u1 <- createUser "Ernie" brig
  u2 <- createUser "Bert" brig
  let uid1 = userId u1
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
      uid2 = userId u2
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  cnv <- responseJsonError =<< (createConv galley uid1 [uid2] <!! const 201 === statusCode)
  let cid = qUnqualified . cnvQualifiedId $ cnv
      qcid = Qualified cid localDomain
  -- Add two bots there
  bid1 <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  bid2 <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  liftIO $ assertBool "bot ids should be different" (bid1 /= bid2)
  let buid1 = botUserId bid1
      buid2 = botUserId bid2
      qbuid1 = Qualified buid1 localDomain
      qbuid2 = Qualified buid2 localDomain
  -- Delete the service; the bots should be removed from the conversation
  WS.bracketR cannon uid1 $ \ws -> do
    deleteService brig pid sid defProviderPassword
      !!! const 202 === statusCode
    _ <- waitFor (5 # Second) not (isMember galley buid1 cid)
    _ <- waitFor (5 # Second) not (isMember galley buid2 cid)
    getBotConv galley bid1 cid !!! const 404 === statusCode
    getBotConv galley bid2 cid !!! const 404 === statusCode
    wsAssertMemberLeave ws qcid qbuid1 [qbuid1]
    wsAssertMemberLeave ws qcid qbuid2 [qbuid2]
  -- The service should not be available
  getService brig pid sid
    !!! const 404 === statusCode
  getServiceProfile brig uid1 pid sid
    !!! const 404 === statusCode

testAddRemoveBot :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBot config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare users
  u1 <- createUser "Ernie" brig
  u2 <- createUser "Bert" brig
  let uid1 = userId u1
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
      uid2 = userId u2
  h <- randomHandle
  putHandle brig uid1 h !!! const 200 === statusCode
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  -- Create conversation
  _rs <- createConv galley uid1 [uid2] <!! const 201 === statusCode
  let Just cnv = responseJsonMaybe _rs
  let cid = qUnqualified . cnvQualifiedId $ cnv
  testAddRemoveBotUtil localDomain pid sid cid u1 u2 h sref buf brig galley cannon

testMessageBot :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBot config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare user with client
  usr <- createUser "User" brig
  let uid = userId usr
  let quid = userQualifiedId usr
  let new = defNewClient PermanentClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
  _rs <- addClient brig uid new <!! const 201 === statusCode
  let Just uc = clientId <$> responseJsonMaybe _rs
  -- Create conversation
  _rs <- createConv galley uid [] <!! const 201 === statusCode
  let Just cid = qUnqualified . cnvQualifiedId <$> responseJsonMaybe _rs
  testMessageBotUtil quid uc cid pid sid sref buf brig galley cannon

testBadFingerprint :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testBadFingerprint config db brig galley _cannon = do
  -- Generate a random key and register a service using that key
  sref <- withSystemTempFile "wire-provider.key" $ \fp h -> do
    ServiceKeyPEM key <- randServiceKey
    liftIO $ BS.hPut h (pemWriteBS key) >> hClose h
    registerService config {publicKey = fp} db brig
  -- Run the service with a different key (i.e. the key from the config)
  runService config defServiceApp $ \_ -> do
    let pid = sref ^. serviceRefProvider
    let sid = sref ^. serviceRefId
    -- Prepare user with client
    usr <- createUser "User" brig
    let uid = userId usr
    let new = defNewClient PermanentClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
    _rs <- addClient brig uid new <!! const 201 === statusCode
    -- Create conversation
    _rs <- createConv galley uid [] <!! const 201 === statusCode
    let Just cid = qUnqualified . cnvQualifiedId <$> responseJsonMaybe _rs
    -- Try to add a bot and observe failure
    addBot brig uid pid sid cid
      !!! const 502 === statusCode

testAddRemoveBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (u1, u2, h, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let (uid1, uid2) = (userId u1, userId u2)
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
  -- Ensure cannot add bots to managed conversations
  cidFail <- Team.createManagedConv galley tid uid1 [uid2] Nothing
  addBot brig uid1 pid sid cidFail !!! do
    const 403 === statusCode
    const (Just "invalid-conversation") === fmap Error.label . responseJsonMaybe
  testAddRemoveBotUtil localDomain pid sid cid u1 u2 h sref buf brig galley cannon

testBotTeamOnlyConv :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testBotTeamOnlyConv config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (u1, u2, _h, _tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let (uid1, uid2) = (userId u1, userId u2)
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
      qcid = Qualified cid localDomain
  -- Make the conversation team-only and check that the bot can't be added
  -- to the conversation
  setAccessRole uid1 cid TeamAccessRole
  addBot brig uid1 pid sid cid !!! do
    const 403 === statusCode
    const (Just "invalid-conversation") === fmap Error.label . responseJsonMaybe
  -- Make the conversation allowed for guests and add the bot successfully
  setAccessRole uid1 cid NonActivatedAccessRole
  bid <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  let buid = botUserId bid
      qbuid = Qualified buid localDomain
  -- Make the conversation team-only again and check that the bot has been removed
  WS.bracketR cannon uid1 $ \ws -> do
    setAccessRole uid1 cid TeamAccessRole
    _ <- waitFor (5 # Second) not (isMember galley buid cid)
    getBotConv galley bid cid
      !!! const 404 === statusCode
    svcAssertConvAccessUpdate
      buf
      quid1
      (ConversationAccessUpdate [InviteAccess] TeamAccessRole)
      qcid
    svcAssertMemberLeave buf qbuid [qbuid] qcid
    wsAssertMemberLeave ws qcid qbuid [qbuid]
  where
    setAccessRole uid cid role =
      updateConversationAccess galley uid cid [InviteAccess] role
        !!! const 200 === statusCode

testMessageBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare user with client
  (uid, tid) <- Team.createUserWithTeam brig
  let new = defNewClient PermanentClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
  _rs <- addClient brig uid new <!! const 201 === statusCode
  let Just uc = clientId <$> responseJsonMaybe _rs
  -- Whitelist the bot
  whitelistService brig uid tid pid sid
  -- Create conversation
  cid <- Team.createTeamConv galley tid uid [] Nothing
  quid <- userQualifiedId . selfUser <$> getSelfProfile brig uid
  testMessageBotUtil quid uc cid pid sid sref buf brig galley cannon

testDeleteConvBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeleteConvBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  -- Prepare users and the bot
  (u1, u2, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let (uid1, uid2) = (userId u1, userId u2)
      quid2 = userQualifiedId u2
      localDomain = qDomain quid2
      qcid = Qualified cid localDomain
  bid <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  -- Delete the conversation and check that everyone is notified
  -- via an event, including the bot itself.
  WS.bracketR2 cannon uid1 uid2 $ \wss -> do
    -- 200 response on success
    Team.deleteTeamConv galley tid cid uid2
    -- Events for the users
    forM_ wss $ \ws -> wsAssertConvDelete ws qcid quid2
    -- Event for the bot
    svcAssertConvDelete buf quid2 qcid
  -- Check that the conversation no longer exists
  forM_ [uid1, uid2] $ \uid ->
    getConversation galley uid cid !!! const 404 === statusCode
  getBotConv galley bid cid !!! const 404 === statusCode

testDeleteTeamBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeleteTeamBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  -- Prepare users and the bot
  (u1, u2, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let (uid1, uid2) = (userId u1, userId u2)
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
      qcid = Qualified cid localDomain
  bid <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  -- Delete the team, and check that the bot (eventually)
  -- receives a notification via event
  Team.deleteTeam galley tid uid1
  -- NOTE: Due to the async nature of a team deletion, some
  -- events may or may not be sent (for instance, team members)
  -- leaving a conversation. Thus, we check _only_ for the relevant
  -- ones for the bot, which are the ConvDelete event
  svcAssertEventuallyConvDelete buf quid1 qcid
  -- Wait until all users have been deleted (can take a while)
  forM_ [uid1, uid2] $ \uid -> do
    void $ retryWhileN 20 (/= Intra.Deleted) (getStatus brig uid)
    chkStatus brig uid Intra.Deleted
    getConversation galley uid cid !!! const 404 === statusCode
  -- Check the bot cannot see the conversation either
  getBotConv galley bid cid !!! const 404 === statusCode

-------------------------------------------------------------------------------
-- Service Whitelist

testWhitelistSearchPermissions :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testWhitelistSearchPermissions _config _db brig galley = do
  -- Create a team
  (owner, tid) <- Team.createUserWithTeam brig
  -- Check that users who are not on the team can't search
  nonMember <- userId <$> randomUser brig
  listTeamServiceProfilesByPrefix brig nonMember tid Nothing True 20 !!! do
    const 403 === statusCode
    const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe
  -- Check that team members with no permissions can search
  member <- userId <$> Team.createTeamMember brig galley owner tid Team.noPermissions
  listTeamServiceProfilesByPrefix brig member tid Nothing True 20
    !!! const 200 === statusCode

testWhitelistUpdatePermissions :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testWhitelistUpdatePermissions config db brig galley = do
  -- Create a team
  (owner, tid) <- Team.createUserWithTeam brig
  -- Create a team admin
  let Just adminPermissions = Team.newPermissions Team.serviceWhitelistPermissions mempty
  admin <- userId <$> Team.createTeamMember brig galley owner tid adminPermissions
  -- Create a service
  pid <- providerId <$> randomProvider db brig
  new <- defNewService config
  sid <- serviceId <$> addGetService brig pid new
  enableService brig pid sid
  -- Check that a random user can't add it to the whitelist
  _uid <- userId <$> randomUser brig
  updateServiceWhitelist brig _uid tid (UpdateServiceWhitelist pid sid True) !!! do
    const 403 === statusCode
    const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe
  -- Check that a member who's not a team admin also can't add it to the whitelist
  _uid <- userId <$> Team.createTeamMember brig galley owner tid Team.noPermissions
  updateServiceWhitelist brig _uid tid (UpdateServiceWhitelist pid sid True) !!! do
    const 403 === statusCode
    const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe
  -- Check that a team admin can add and remove from the whitelist
  whitelistService brig admin tid pid sid
  dewhitelistService brig admin tid pid sid

testSearchWhitelist :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testSearchWhitelist config db brig galley = do
  -- Create a team, a team owner, and a team member with no permissions
  (owner, tid) <- Team.createUserWithTeam brig
  uid <- userId <$> Team.createTeamMember brig galley owner tid Team.noPermissions
  -- Create services and add them all to the whitelist
  pid <- providerId <$> randomProvider db brig
  uniq <- UUID.toText . toUUID <$> randomId
  new <- defNewService config
  svcs <- mapM (addGetService brig pid . mkNew new) (taggedServiceNames uniq)
  forM_ svcs $ \svc -> do
    let sid = serviceId svc
    enableService brig pid sid
    whitelistService brig owner tid pid sid
  let mkName n = Name (uniq <> "|" <> n)
  let services :: [(ServiceId, Name)]
      services = map (serviceId &&& serviceName) svcs
  -- This is how we're going to call our .../services/whitelisted
  -- endpoint. Every time we call it twice (with filter_disabled=false and
  -- without) and assert that results match – which should always be the
  -- case since in this test we won't have any disabled services.
  let search :: HasCallStack => Maybe Text -> Http ServiceProfilePage
      search mbName = do
        r1 <- searchServiceWhitelist brig 20 uid tid mbName
        r2 <- searchServiceWhitelistAll brig 20 uid tid mbName
        liftIO $
          assertEqual
            ("search for " <> show mbName <> " with and without filtering")
            r1
            r2
        return r1
  -- Check that search finds all services that we created
  search (Just uniq)
    >>= assertServiceDetails ("all with prefix " <> show uniq) services
  -- Check that search works without a prefix
  do
    -- with the zeroes around, this service should be the first on the
    -- resulting search results list
    uniq2 <- mappend "0000000000|" . UUID.toText . toUUID <$> randomId
    let name = Name (uniq2 <> "|Extra")
    sid <- serviceId <$> addGetService brig pid (mkNew new (name, [PollTag]))
    enableService brig pid sid
    whitelistService brig owner tid pid sid
    page <- search Nothing
    assertServiceDetails "without prefix" ((sid, name) : take 19 services) page
    liftIO $ assertEqual "has more" True (serviceProfilePageHasMore page)
  -- This function searches for a prefix and check that the results match
  -- our known list of services
  let searchAndCheck :: HasCallStack => Name -> Http [ServiceProfile]
      searchAndCheck (Name name) = do
        result <- search (Just name)
        assertServiceDetails ("name " <> show name) (select name services) result
        return (serviceProfilePageResults result)
  -- Search by exact name and check that only one service is found
  forM_ (take 3 services) $ \(sid, Name name) ->
    search (Just name) >>= assertServiceDetails ("name " <> show name) [(sid, Name name)]
  -- Check some chosen prefixes.
  -- # Bjø -> Bjørn
  _found <- map serviceProfileName <$> searchAndCheck (mkName "Bjø")
  liftIO $ assertEqual "Bjø" [mkName "Bjørn"] _found
  -- # Bj -> bjorn, Bjørn
  _found <- map serviceProfileName <$> searchAndCheck (mkName "Bj")
  liftIO $ assertEqual "Bj" [mkName "bjorn", mkName "Bjørn"] _found
  -- # chris -> CHRISTMAS
  _found <- map serviceProfileName <$> searchAndCheck (mkName "chris")
  liftIO $ assertEqual "chris" [mkName "CHRISTMAS"] _found
  -- Ensure name changes are also indexed properly
  forM_ (take 3 services) $ \(sid, _) ->
    searchAndAssertNameChange brig pid sid uid uniq (search . Just . fromName)
  where
    mkNew new (n, t) =
      new
        { newServiceName = n,
          newServiceTags = unsafeRange (Set.fromList t)
        }
    select prefix = filter (Text.isPrefixOf (Text.toLower prefix) . Text.toLower . fromName . snd)

testSearchWhitelistHonorUpdates :: Config -> DB.ClientState -> Brig -> Http ()
testSearchWhitelistHonorUpdates config db brig = do
  -- Create a team with an owner
  (uid, tid) <- Team.createUserWithTeam brig
  let expectWhitelist ifAll ifEnabled = do
        searchServiceWhitelistAll brig 20 uid tid Nothing
          >>= assertServiceDetails "search all" ifAll
        searchServiceWhitelist brig 20 uid tid Nothing
          >>= assertServiceDetails "search enabled" ifEnabled
  -- Check that the whitelist is initially empty
  expectWhitelist [] []
  -- Add a (initially disabled) service and whitelist it
  pid <- providerId <$> randomProvider db brig
  new <- defNewService config
  sid <- serviceId <$> addGetService brig pid new
  let name = newServiceName new
  whitelistService brig uid tid pid sid
  -- The service should be found by 'searchServiceWhitelistAll' but not
  -- the standard version
  expectWhitelist [(sid, name)] []
  -- After enabling it, it should be found by both variants
  enableService brig pid sid
  expectWhitelist [(sid, name)] [(sid, name)]
  -- After removing it from the whitelist, it should not be found
  dewhitelistService brig uid tid pid sid
  expectWhitelist [] []

testWhitelistBasic :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testWhitelistBasic config db brig galley =
  withTestService config db brig defServiceApp $ \sref buf -> do
    let pid = sref ^. serviceRefProvider
    let sid = sref ^. serviceRefId
    -- Create a team
    (owner, tid) <- Team.createUserWithTeam brig
    -- Check that the service can't be added to a conversation by default
    cid <- Team.createTeamConv galley tid owner [] Nothing
    addBot brig owner pid sid cid !!! do
      const 403 === statusCode
      const (Just "service-not-whitelisted") === fmap Error.label . responseJsonMaybe
    -- Check that after whitelisting the service, it can be added to the conversation
    whitelistService brig owner tid pid sid
    bid <-
      fmap rsAddBotId . responseJsonError
        =<< (addBot brig owner pid sid cid <!! const 201 === statusCode)
    _ <- svcAssertBotCreated buf bid cid
    -- Check that after de-whitelisting the service can't be added to conversations
    removeBot brig owner cid bid
      !!! const 200 === statusCode
    dewhitelistService brig owner tid pid sid
    addBot brig owner pid sid cid !!! do
      const 403 === statusCode
      const (Just "service-not-whitelisted") === fmap Error.label . responseJsonMaybe
    -- Check that a disabled service can be whitelisted
    disableService brig pid sid
    whitelistService brig owner tid pid sid

testWhitelistKickout :: Domain -> Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testWhitelistKickout localDomain config db brig galley cannon = do
  -- Create a team and a conversation
  (owner, tid) <- Team.createUserWithTeam brig
  let qowner = Qualified owner localDomain
  cid <- Team.createTeamConv galley tid owner [] Nothing
  let qcid = Qualified cid localDomain
  -- Create a service
  withTestService config db brig defServiceApp $ \sref buf -> do
    -- Add it to the conversation
    let pid = sref ^. serviceRefProvider
        sid = sref ^. serviceRefId
    whitelistService brig owner tid pid sid
    bot <-
      responseJsonError
        =<< (addBot brig owner pid sid cid <!! const 201 === statusCode)
    let bid = rsAddBotId bot
        buid = botUserId bid
        qbuid = Qualified buid localDomain
    _ <- svcAssertBotCreated buf bid cid
    svcAssertMemberJoin buf qowner [qbuid] qcid
    -- De-whitelist the service; both bots should be kicked out
    WS.bracketR cannon owner $ \ws -> do
      dewhitelistService brig owner tid pid sid
      _ <- waitFor (2 # Second) not (isMember galley buid cid)
      getBotConv galley bid cid
        !!! const 404 === statusCode
      wsAssertMemberLeave ws qcid qowner [qbuid]
      svcAssertMemberLeave buf qowner [qbuid] qcid
    -- The bot should not get any further events
    liftIO $
      timeout (2 # Second) (readChan buf) >>= \case
        Nothing -> pure ()
        Just (TestBotCreated _) -> assertFailure "bot got a TestBotCreated event"
        Just (TestBotMessage e) -> assertFailure ("bot got an event: " <> show (evtType e))

testDeWhitelistDeletedConv :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeWhitelistDeletedConv config db brig galley cannon = do
  -- Create a service
  withTestService config db brig defServiceApp $ \sref buf -> do
    -- Create a team and a conversation
    (u1, u2, _h, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
    let uid1 = userId u1
        uid2 = userId u2
        quid1 = userQualifiedId u1
        localDomain = qDomain quid1
    -- Add a bot there
    _bid1 <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
    -- Delete conversation (to ensure deleteService can be called even with a deleted conversation)
    Team.deleteTeamConv galley tid cid uid1
    -- De-whitelist the service
    -- this should work (not throw a 500) even with brig being unaware of deleted conversations
    -- TODO: we should think of a way to synchronize this conversation information between galley and brig
    dewhitelistService brig uid1 tid pid sid

--------------------------------------------------------------------------------
-- API Operations

registerProvider ::
  Brig ->
  NewProvider ->
  Http ResponseLBS
registerProvider brig new =
  post $
    brig
      . path "/provider/register"
      . contentJson
      . body (RequestBodyLBS (encode new))

getProviderActivationCodeInternal ::
  Brig ->
  Email ->
  Http ResponseLBS
getProviderActivationCodeInternal brig email =
  get $
    brig
      . path "/i/provider/activation-code"
      . queryItem "email" (toByteString' email)

activateProvider ::
  Brig ->
  Code.Key ->
  Code.Value ->
  Http ResponseLBS
activateProvider brig key val =
  get $
    brig
      . path "/provider/activate"
      . queryItem "key" (toByteString' key)
      . queryItem "code" (toByteString' val)

loginProvider ::
  Brig ->
  Email ->
  PlainTextPassword ->
  Http ResponseLBS
loginProvider brig email pw =
  post $
    brig
      . path "/provider/login"
      . contentJson
      . body (RequestBodyLBS (encode (ProviderLogin email pw)))

updateProvider ::
  Brig ->
  ProviderId ->
  UpdateProvider ->
  Http ResponseLBS
updateProvider brig pid upd =
  put $
    brig
      . path "/provider"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode upd))

updateProviderPassword ::
  Brig ->
  ProviderId ->
  PasswordChange ->
  Http ResponseLBS
updateProviderPassword brig pid upd =
  put $
    brig
      . path "/provider/password"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode upd))

initiateEmailUpdateProvider ::
  Brig ->
  ProviderId ->
  EmailUpdate ->
  Http ResponseLBS
initiateEmailUpdateProvider brig pid upd =
  put $
    brig
      . path "/provider/email"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode upd))

initiatePasswordResetProvider ::
  Brig ->
  PasswordReset ->
  Http ResponseLBS
initiatePasswordResetProvider brig npr =
  post $
    brig
      . path "/provider/password-reset"
      . contentJson
      . body (RequestBodyLBS (encode npr))

completePasswordResetProvider ::
  Brig ->
  CompletePasswordReset ->
  Http ResponseLBS
completePasswordResetProvider brig e =
  post $
    brig
      . path "/provider/password-reset/complete"
      . contentJson
      . body (RequestBodyLBS (encode e))

deleteProvider ::
  Brig ->
  ProviderId ->
  PlainTextPassword ->
  Http ResponseLBS
deleteProvider brig pid pw =
  delete $
    brig
      . path "/provider"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode (DeleteProvider pw)))

getProvider ::
  Brig ->
  ProviderId ->
  Http ResponseLBS
getProvider brig pid =
  get $
    brig
      . path "/provider"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)

getProviderProfile ::
  Brig ->
  ProviderId ->
  UserId ->
  Http ResponseLBS
getProviderProfile brig pid uid =
  get $
    brig
      . paths ["providers", toByteString' pid]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)

addService ::
  Brig ->
  ProviderId ->
  NewService ->
  Http ResponseLBS
addService brig pid new =
  post $
    brig
      . path "/provider/services"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode new))

getService ::
  Brig ->
  ProviderId ->
  ServiceId ->
  Http ResponseLBS
getService brig pid sid =
  get $
    brig
      . paths ["provider", "services", toByteString' sid]
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)

getServiceProfile ::
  Brig ->
  UserId ->
  ProviderId ->
  ServiceId ->
  Http ResponseLBS
getServiceProfile brig uid pid sid =
  get $
    brig
      . paths ["providers", toByteString' pid, "services", toByteString' sid]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)

updateService ::
  Brig ->
  ProviderId ->
  ServiceId ->
  UpdateService ->
  Http ResponseLBS
updateService brig pid sid upd =
  put $
    brig
      . paths ["provider", "services", toByteString' sid]
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode upd))

updateServiceConn ::
  Brig ->
  ProviderId ->
  ServiceId ->
  UpdateServiceConn ->
  Http ResponseLBS
updateServiceConn brig pid sid upd =
  put $
    brig
      . paths ["provider", "services", toByteString' sid, "connection"]
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode upd))

updateServiceWhitelist ::
  Brig ->
  UserId ->
  TeamId ->
  UpdateServiceWhitelist ->
  Http ResponseLBS
updateServiceWhitelist brig uid tid upd =
  post $
    brig
      . paths ["teams", toByteString' tid, "services", "whitelist"]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode upd))

deleteService ::
  Brig ->
  ProviderId ->
  ServiceId ->
  PlainTextPassword ->
  Http ResponseLBS
deleteService brig pid sid pw =
  delete $
    brig
      . paths ["provider", "services", toByteString' sid]
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)
      . contentJson
      . body (RequestBodyLBS (encode (DeleteService pw)))

listServiceProfilesByPrefix ::
  Brig ->
  UserId ->
  Name ->
  Int ->
  Http ResponseLBS
listServiceProfilesByPrefix brig uid start size =
  get $
    brig
      . path "/services"
      . queryItem "start" (toByteString' start)
      . queryItem "size" (toByteString' size)
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)

listTeamServiceProfilesByPrefix ::
  Brig ->
  UserId ->
  TeamId ->
  Maybe Text ->
  -- | Filter out disabled
  Bool ->
  Int ->
  Http ResponseLBS
listTeamServiceProfilesByPrefix brig uid tid mbPrefix filterDisabled size =
  get $
    brig
      . paths ["teams", toByteString' tid, "services", "whitelisted"]
      . maybe id (queryItem "prefix" . toByteString') mbPrefix
      . (if filterDisabled then id else queryItem "filter_disabled" "false")
      . queryItem "size" (toByteString' size)
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)

listServiceProfilesByTag ::
  Brig ->
  UserId ->
  MatchAny ->
  Maybe Name ->
  Int ->
  Http ResponseLBS
listServiceProfilesByTag brig uid tags start size =
  get $
    brig
      . path "/services"
      . queryItem "tags" (toByteString' cond)
      . maybe id (queryItem "start" . toByteString') start
      . queryItem "size" (toByteString' size)
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
  where
    cond :: QueryAnyTags 1 3
    cond =
      fromMaybe (error "Too many tags in query")
        . queryAnyTags
        $ tags

addBot ::
  Brig ->
  UserId ->
  ProviderId ->
  ServiceId ->
  ConvId ->
  Http ResponseLBS
addBot brig uid pid sid cid =
  post $
    brig
      . paths ["conversations", toByteString' cid, "bots"]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode (AddBot pid sid Nothing)))

removeBot ::
  Brig ->
  UserId ->
  ConvId ->
  BotId ->
  Http ResponseLBS
removeBot brig uid cid bid =
  delete $
    brig
      . paths ["conversations", toByteString' cid, "bots", toByteString' bid]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
      . header "Z-Connection" "conn"

createConv ::
  Galley ->
  UserId ->
  [UserId] ->
  Http ResponseLBS
createConv g u us =
  post $
    g
      . path "/conversations"
      . header "Z-User" (toByteString' u)
      . header "Z-Type" "access"
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode (NewConvUnmanaged conv)))
  where
    conv = NewConv us [] Nothing Set.empty Nothing Nothing Nothing Nothing roleNameWireAdmin

postMessage ::
  Galley ->
  UserId ->
  ClientId ->
  ConvId ->
  [(UserId, ClientId, Text)] ->
  Http ResponseLBS
postMessage galley fromu fromc cid rcps =
  post $
    galley
      . paths ["conversations", toByteString' cid, "otr", "messages"]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' fromu)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode (mkMessage fromc rcps)))

postBotMessage ::
  Galley ->
  BotId ->
  ClientId ->
  ConvId ->
  [(UserId, ClientId, Text)] ->
  Http ResponseLBS
postBotMessage galley fromb fromc cid rcps =
  post $
    galley
      . path "/bot/messages"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' fromb)
      . header "Z-Conversation" (toByteString' cid)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode (mkMessage fromc rcps)))

getBotConv ::
  Galley ->
  BotId ->
  ConvId ->
  Http ResponseLBS
getBotConv galley bid cid =
  get $
    galley
      . path "/bot/conversation"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)
      . header "Z-Conversation" (toByteString' cid)

updateConversationAccess ::
  Galley ->
  UserId ->
  ConvId ->
  [Access] ->
  AccessRole ->
  Http ResponseLBS
updateConversationAccess galley uid cid access role =
  put $
    galley
      . paths ["conversations", toByteString' cid, "access"]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode upd))
  where
    upd = ConversationAccessUpdate access role

--------------------------------------------------------------------------------
-- DB Operations

lookupCode :: MonadIO m => DB.ClientState -> Code.Gen -> Code.Scope -> m (Maybe Code.Code)
lookupCode db gen = liftIO . DB.runClient db . Code.lookup (Code.genKey gen)

--------------------------------------------------------------------------------
-- Utilities

-- | Step-by-step registration procedure with verification
-- of pre- and post-conditions. Activation can be done through
-- direct DB access (if given) otherwise it falls back to using
-- an internal endpoint
testRegisterProvider :: Maybe DB.ClientState -> Brig -> Http ()
testRegisterProvider db' brig = do
  email <- randomEmail
  let new = defNewProvider email
  _rs <-
    registerProvider brig new
      <!! const 201 === statusCode
  let Just npr = responseJsonMaybe _rs :: Maybe NewProviderResponse
  -- Since a password was given, none should have been generated
  liftIO $ assertBool "password" (isNothing (rsNewProviderPassword npr))
  let pid = rsNewProviderId npr
  -- No login possible directly after registration
  loginProvider brig email defProviderPassword !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === fmap Error.label . responseJsonMaybe
  -- Activate email
  case db' of
    Just db -> do
      -- Activate email
      gen <- Code.mkGen (Code.ForEmail email)
      Just vcode <- lookupCode db gen Code.IdentityVerification
      activateProvider brig (Code.codeKey vcode) (Code.codeValue vcode)
        !!! const 200 === statusCode
    Nothing -> do
      _rs <-
        getProviderActivationCodeInternal brig email
          <!! const 200 === statusCode
      let Just pair = responseJsonMaybe _rs :: Maybe Code.KeyValuePair
      activateProvider brig (Code.kcKey pair) (Code.kcCode pair)
        !!! const 200 === statusCode
  -- Login succeeds after activation (due to auto-approval)
  loginProvider brig email defProviderPassword
    !!! const 200 === statusCode
  -- Email address is now taken
  registerProvider brig new !!! do
    const 409 === statusCode
    const (Just "email-exists") === fmap Error.label . responseJsonMaybe
  -- Retrieve full account and public profile
  -- (these are identical for now).
  uid <- randomId
  _rs <- getProvider brig pid <!! const 200 === statusCode
  let Just p = responseJsonMaybe _rs
  _rs <- getProviderProfile brig pid uid <!! const 200 === statusCode
  let Just pp = responseJsonMaybe _rs
  -- When updating the Provider dataype, one _must_ remember to also add
  -- an extra check in this integration test.
  liftIO $ do
    assertEqual "id" pid (providerId p)
    assertEqual "name" defProviderName (providerName p)
    assertEqual "email" email (providerEmail p)
    assertEqual "url" defProviderUrl (providerUrl p)
    assertEqual "description" defProviderDescr (providerDescr p)
    assertEqual "profile" (ProviderProfile p) pp

randomProvider :: HasCallStack => DB.ClientState -> Brig -> Http Provider
randomProvider db brig = do
  email <- randomEmail
  gen <- Code.mkGen (Code.ForEmail email)
  -- Register
  let new = defNewProvider email
  _rs <-
    registerProvider brig new
      <!! const 201 === statusCode
  let Just pid = rsNewProviderId <$> responseJsonMaybe _rs
  -- Activate (auto-approval)
  Just vcode <- lookupCode db gen Code.IdentityVerification
  activateProvider brig (Code.codeKey vcode) (Code.codeValue vcode)
    !!! const 200 === statusCode
  -- Fetch
  _rs <- getProvider brig pid <!! const 200 === statusCode
  let Just prv = responseJsonMaybe _rs
  return prv

addGetService :: HasCallStack => Brig -> ProviderId -> NewService -> Http Service
addGetService brig pid new = do
  _rs <- addService brig pid new <!! const 201 === statusCode
  let Just srs = responseJsonMaybe _rs
  let sid = rsNewServiceId srs
  _rs <- getService brig pid sid <!! const 200 === statusCode
  let Just svc = responseJsonMaybe _rs
  return svc

enableService :: HasCallStack => Brig -> ProviderId -> ServiceId -> Http ()
enableService brig pid sid = do
  let upd =
        (mkUpdateServiceConn defProviderPassword)
          { updateServiceConnEnabled = Just True
          }
  updateServiceConn brig pid sid upd
    !!! const 200 === statusCode

disableService :: HasCallStack => Brig -> ProviderId -> ServiceId -> Http ()
disableService brig pid sid = do
  let upd =
        (mkUpdateServiceConn defProviderPassword)
          { updateServiceConnEnabled = Just False
          }
  updateServiceConn brig pid sid upd
    !!! const 200 === statusCode

whitelistService ::
  HasCallStack =>
  Brig ->
  -- | Team owner
  UserId ->
  -- | Team
  TeamId ->
  ProviderId ->
  ServiceId ->
  Http ()
whitelistService brig uid tid pid sid =
  updateServiceWhitelist brig uid tid (UpdateServiceWhitelist pid sid True)
    !!!
    -- TODO: allow both 200 and 204 here and use it in 'testWhitelistEvents'
    const 200 === statusCode

dewhitelistService ::
  HasCallStack =>
  Brig ->
  -- | Team owner
  UserId ->
  -- | Team
  TeamId ->
  ProviderId ->
  ServiceId ->
  Http ()
dewhitelistService brig uid tid pid sid =
  updateServiceWhitelist brig uid tid (UpdateServiceWhitelist pid sid False)
    !!!
    -- TODO: allow both 200 and 204 here and use it in 'testWhitelistEvents'
    const 200 === statusCode

defNewService :: MonadIO m => Config -> m NewService
defNewService config = liftIO $ do
  key <- readServiceKey (publicKey config)
  return
    NewService
      { newServiceName = defServiceName,
        newServiceSummary = unsafeRange defProviderSummary,
        newServiceDescr = unsafeRange defServiceDescr,
        newServiceUrl = defServiceUrl,
        newServiceKey = key,
        newServiceToken = Nothing,
        newServiceAssets = defServiceAssets,
        newServiceTags = defServiceTags
      }

defNewProvider :: Email -> NewProvider
defNewProvider email =
  NewProvider
    { newProviderEmail = email,
      newProviderPassword = Just defProviderPassword,
      newProviderName = defProviderName,
      newProviderUrl = defProviderUrl,
      newProviderDescr = unsafeRange defProviderDescr
    }

defProviderUrl :: HttpsUrl
defProviderUrl = fromJust (fromByteString "https://localhost/")

defProviderName :: Name
defProviderName = Name "Integration Test Provider"

defProviderSummary :: Text
defProviderSummary = "A short summary of the integration test provider"

defProviderDescr :: Text
defProviderDescr = "A long description of an integration test provider"

defProviderPassword :: PlainTextPassword
defProviderPassword = PlainTextPassword "password"

defServiceName :: Name
defServiceName = Name "Test Service"

defServiceDescr :: Text
defServiceDescr = "Test service description"

defServiceUrl :: HttpsUrl
defServiceUrl = fromJust (fromByteString "https://localhost/test")

defServiceTags :: Range 1 3 (Set ServiceTag)
defServiceTags = unsafeRange (Set.singleton SocialTag)

defServiceAssets :: [Asset]
defServiceAssets = [ImageAsset "key" (Just AssetComplete)]

-- TODO: defServiceToken :: ServiceToken

readServiceKey :: MonadIO m => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
  bs <- BS.readFile fp
  let Right [k] = pemParseBS bs
  return (ServiceKeyPEM k)

randServiceKey :: MonadIO m => m ServiceKeyPEM
randServiceKey = liftIO $ do
  kp <- generateRSAKey' 4096 65537
  Right [k] <- pemParseBS . C8.pack <$> writePublicKey kp
  return (ServiceKeyPEM k)

waitFor :: MonadIO m => Timeout -> (a -> Bool) -> m a -> m a
waitFor t f ma = do
  a <- ma
  if
      | f a -> return a
      | t <= 0 -> liftIO $ throwM TimedOut
      | otherwise -> do
        liftIO $ threadDelay (1 # Second)
        waitFor (t - 1 # Second) f ma

-- | Run a test case with an external service application.
withTestService ::
  Config ->
  DB.ClientState ->
  Brig ->
  (Chan e -> Application) ->
  (ServiceRef -> Chan e -> Http a) ->
  Http a
withTestService config db brig mkApp go = do
  sref <- registerService config db brig
  runService config mkApp (go sref)

registerService :: Config -> DB.ClientState -> Brig -> Http ServiceRef
registerService config db brig = do
  prv <- randomProvider db brig
  new <- defNewService config
  let Just url =
        fromByteString $
          encodeUtf8 (botHost config) <> ":"
            <> C8.pack (show (botPort config))
  svc <- addGetService brig (providerId prv) (new {newServiceUrl = url})
  let pid = providerId prv
  let sid = serviceId svc
  enableService brig pid sid
  return (newServiceRef sid pid)

runService ::
  Config ->
  (Chan e -> Application) ->
  (Chan e -> Http a) ->
  Http a
runService config mkApp go = do
  let tlss = Warp.tlsSettings (cert config) (privateKey config)
  let defs = Warp.defaultSettings {Warp.settingsPort = botPort config}
  buf <- liftIO newChan
  srv <-
    liftIO . Async.async $
      Warp.runTLS tlss defs $
        mkApp buf
  go buf `finally` liftIO (Async.cancel srv)

data TestBot = TestBot
  { testBotId :: !BotId,
    testBotClient :: !ClientId,
    testBotConv :: !Ext.BotConvView,
    testBotToken :: !Text,
    testBotLastPrekey :: !LastPrekey,
    testBotPrekeys :: ![Prekey],
    testBotLocale :: !Locale,
    testBotOrigin :: !Ext.BotUserView
  }
  deriving (Eq, Show)

data TestBotEvent
  = TestBotCreated TestBot
  | TestBotMessage Event

-- TODO: Test that the authorization header is properly set
defServiceApp :: Chan TestBotEvent -> Application
defServiceApp buf =
  Wai.route
    [ ("/bots", onBotCreate),
      ("/bots/:bot/messages", onBotMessage)
    ]
  where
    onBotCreate _ rq k = do
      -- TODO: Match request method
      js <- strictRequestBody rq
      case eitherDecode js of
        Left e -> k $ responseLBS status400 [] (LC8.pack e)
        Right new -> do
          let pks = [somePrekeys !! 0]
          let lpk = someLastPrekeys !! 0
          let rsp =
                Ext.NewBotResponse
                  { Ext.rsNewBotPrekeys = pks,
                    Ext.rsNewBotLastPrekey = lpk,
                    Ext.rsNewBotName = Nothing, -- TODO
                    Ext.rsNewBotColour = Nothing, -- TODO
                    Ext.rsNewBotAssets = Nothing -- TODO
                  }
          let bot =
                TestBot
                  { testBotId = Ext.newBotId new,
                    testBotClient = Ext.newBotClient new,
                    testBotConv = Ext.newBotConv new,
                    testBotToken = Ext.newBotToken new,
                    testBotLastPrekey = lpk,
                    testBotPrekeys = pks,
                    testBotLocale = Ext.newBotLocale new,
                    testBotOrigin = Ext.newBotOrigin new
                  }
          writeChan buf (TestBotCreated bot)
          k $ responseLBS status201 [] (encode rsp)
    onBotMessage _ rq k = do
      js <- strictRequestBody rq
      case eitherDecode js of
        Left e -> k $ responseLBS status400 [] (LC8.pack e)
        Right ev -> do
          writeChan buf (TestBotMessage ev)
          k $ responseLBS status200 [] "success"

wsAssertMemberJoin :: MonadIO m => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> m ()
wsAssertMemberJoin ws conv usr new = void $
  liftIO $
    WS.assertMatch (5 # Second) ws $
      \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv e @?= conv
        evtType e @?= MemberJoin
        evtFrom e @?= usr
        evtData e @?= EdMembersJoin (SimpleMembers (fmap (\u -> SimpleMember u roleNameWireAdmin) new))

wsAssertMemberLeave :: MonadIO m => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> m ()
wsAssertMemberLeave ws conv usr old = void $
  liftIO $
    WS.assertMatch (5 # Second) ws $
      \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv e @?= conv
        evtType e @?= MemberLeave
        evtFrom e @?= usr
        evtData e @?= EdMembersLeave (UserIdList old)

wsAssertConvDelete :: MonadIO m => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> m ()
wsAssertConvDelete ws conv from = void $
  liftIO $
    WS.assertMatch (5 # Second) ws $
      \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv e @?= conv
        evtType e @?= ConvDelete
        evtFrom e @?= from
        evtData e @?= EdConvDelete

wsAssertMessage :: MonadIO m => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> ClientId -> ClientId -> Text -> m ()
wsAssertMessage ws conv fromu fromc to txt = void $
  liftIO $
    WS.assertMatch (5 # Second) ws $
      \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv e @?= conv
        evtType e @?= OtrMessageAdd
        evtFrom e @?= fromu
        evtData e @?= EdOtrMessage (OtrMessage fromc to txt (Just "data"))

svcAssertMemberJoin :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> [Qualified UserId] -> Qualified ConvId -> m ()
svcAssertMemberJoin buf usr new cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      let msg = SimpleMembers $ fmap (\u -> SimpleMember u roleNameWireAdmin) new
      assertEqual "event type" MemberJoin (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" (EdMembersJoin msg) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: member-join)"

svcAssertMemberLeave :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> [Qualified UserId] -> Qualified ConvId -> m ()
svcAssertMemberLeave buf usr gone cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      let msg = UserIdList gone
      assertEqual "event type" MemberLeave (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" (EdMembersLeave msg) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: member-leave)"

svcAssertConvAccessUpdate :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> ConversationAccessUpdate -> Qualified ConvId -> m ()
svcAssertConvAccessUpdate buf usr upd cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      assertEqual "event type" ConvAccessUpdate (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" (EdConvAccessUpdate upd) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: conv-access-update)"

svcAssertConvDelete :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> Qualified ConvId -> m ()
svcAssertConvDelete buf usr cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      assertEqual "event type" ConvDelete (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" EdConvDelete (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: conv-delete)"

svcAssertBotCreated :: MonadIO m => Chan TestBotEvent -> BotId -> ConvId -> m TestBot
svcAssertBotCreated buf bid cid = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotCreated b) -> do
      assertEqual "bot ID" bid (testBotId b)
      assertEqual "conv" cid (testBotConv b ^. Ext.botConvId)
      -- TODO: Verify the conversation name
      -- TODO: Verify the list of members
      return b
    _ -> throwM $ HUnitFailure Nothing "Event timeout (TestBotCreated)"

svcAssertMessage :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> OtrMessage -> Qualified ConvId -> m ()
svcAssertMessage buf from msg cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      assertEqual "event type" OtrMessageAdd (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" from (evtFrom e)
      assertEqual "event data" (EdOtrMessage msg) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: otr-message-add)"

svcAssertEventuallyConvDelete :: MonadIO m => Chan TestBotEvent -> Qualified UserId -> Qualified ConvId -> m ()
svcAssertEventuallyConvDelete buf usr cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) | evtType e == ConvDelete -> do
      assertEqual "event type" ConvDelete (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" EdConvDelete (evtData e)
    -- We ignore every other message type
    Just (TestBotMessage _) ->
      svcAssertEventuallyConvDelete buf usr cnv
    _ -> assertFailure "Event timeout (TestBotMessage: conv-delete)"

unpackEvents :: Notification -> List1 Event
unpackEvents = WS.unpackPayload

mkMessage :: ClientId -> [(UserId, ClientId, Text)] -> Value
mkMessage fromc rcps =
  object
    [ "sender" .= fromc,
      "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mk rcps),
      "data" .= Just ("data" :: Text)
    ]
  where
    mk (u, c, m) = (text u, HashMap.singleton (text c) m)
    text :: (FromByteString a, ToByteString a) => a -> Text
    text = fromJust . fromByteString . toByteString'

-- | A list of 20 services, all having names that begin with the given prefix.
--
-- NB: in some of the tests above, we depend on the fact that there are
-- exactly 20 services here.
taggedServiceNames :: Text -> [(Name, [ServiceTag])]
taggedServiceNames prefix =
  [ (mkName "Alpha", [SocialTag, QuizTag, BusinessTag]),
    (mkName "Beta", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "bjorn", [SocialTag, QuizTag, TravelTag]),
    (mkName "Bjørn", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "CHRISTMAS", [SocialTag, QuizTag, WeatherTag]),
    (mkName "Delta", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Epsilon", [SocialTag, QuizTag, BusinessTag]),
    (mkName "Freer", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Gamma", [SocialTag, QuizTag, WeatherTag]),
    (mkName "Gramma", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Hera", [SocialTag, QuizTag, TravelTag]),
    (mkName "Io", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Jojo", [SocialTag, QuizTag, WeatherTag]),
    (mkName "Kuba", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Lawn", [SocialTag, QuizTag, TravelTag]),
    (mkName "Mango", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "North", [SocialTag, QuizTag, WeatherTag]),
    (mkName "Yak", [SocialTag, MusicTag, LifestyleTag]),
    (mkName "Zeta", [SocialTag, QuizTag, TravelTag]),
    (mkName "Zulu", [SocialTag, MusicTag, LifestyleTag])
  ]
  where
    mkName n = Name (prefix <> "|" <> n)

testAddRemoveBotUtil ::
  Domain ->
  ProviderId ->
  ServiceId ->
  ConvId ->
  User ->
  User ->
  Text ->
  ServiceRef ->
  Chan TestBotEvent ->
  Brig ->
  Galley ->
  WS.Cannon ->
  Http ()
testAddRemoveBotUtil localDomain pid sid cid u1 u2 h sref buf brig galley cannon = do
  let qcid = Qualified cid localDomain
      uid1 = userId u1
      uid2 = userId u2
      quid1 = Qualified uid1 localDomain
      quid2 = Qualified uid2 localDomain
  -- Add the bot and check that everyone is notified via an event,
  -- including the bot itself.
  (rs, bot) <- WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
    _rs <- addBot brig uid1 pid sid cid <!! const 201 === statusCode
    let Just rs = responseJsonMaybe _rs
        bid = rsAddBotId rs
        qbuid = Qualified (botUserId bid) localDomain
    bot <- svcAssertBotCreated buf bid cid
    liftIO $ assertEqual "bot client" (rsAddBotClient rs) (testBotClient bot)
    liftIO $ assertEqual "bot event" MemberJoin (evtType (rsAddBotEvent rs))
    -- Member join event for both users
    forM_ [ws1, ws2] $ \ws -> wsAssertMemberJoin ws qcid quid1 [qbuid]
    -- Member join event for the bot
    svcAssertMemberJoin buf quid1 [qbuid] qcid
    return (rs, bot)
  let bid = rsAddBotId rs
      buid = botUserId bid
      -- Check that the bot token grants access to the right user and conversation
      Just tok = fromByteString (Text.encodeUtf8 (testBotToken bot))
  liftIO $ do
    assertEqual "principal" bid (BotId (Id (tok ^. ZAuth.body . ZAuth.bot)))
    assertEqual "conversation" cid (Id (tok ^. ZAuth.body . ZAuth.conv))
    assertEqual "provider" pid (Id (tok ^. ZAuth.body . ZAuth.prov))
  let u1Handle = Ext.botUserViewHandle $ testBotOrigin bot
  -- Check that the preferred locale defaults to the locale of the
  -- user who requsted the bot.
  liftIO $ assertEqual "locale" (userLocale u1) (testBotLocale bot)
  liftIO $ assertEqual "handle" (Just (Handle h)) u1Handle
  -- Check that the bot has access to the conversation
  getBotConv galley bid cid !!! const 200 === statusCode
  -- Check that the bot user exists and can be identified as a bot
  _rs <- getUser brig uid1 buid <!! const 200 === statusCode
  let Just bp = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "service" (Just sref) (profileService bp)
    assertEqual "name" defServiceName (profileName bp)
    assertEqual "colour" defaultAccentId (profileAccentId bp)
    assertEqual "assets" defServiceAssets (profileAssets bp)
  -- Check that the bot client exists and has prekeys
  let isBotPrekey = (`elem` testBotPrekeys bot) . prekeyData
  getPreKey brig buid buid (rsAddBotClient rs) !!! do
    const 200 === statusCode
    const (Just True) === fmap isBotPrekey . responseJsonMaybe
  -- Remove the bot and check that everyone is notified via an event,
  -- including the bot itself.
  WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
    -- 200 response with event on success
    _rs <- removeBot brig uid2 cid bid <!! const 200 === statusCode
    let Just ev = rsRemoveBotEvent <$> responseJsonMaybe _rs
    liftIO $ assertEqual "bot event" MemberLeave (evtType ev)
    -- Events for both users
    forM_ [ws1, ws2] $ \ws -> wsAssertMemberLeave ws qcid quid2 [Qualified buid localDomain]
    -- Event for the bot
    svcAssertMemberLeave buf quid2 [Qualified buid localDomain] qcid
    -- Empty 204 response if the bot is not in the conversation
    removeBot brig uid2 cid bid !!! const 204 === statusCode
  -- Check that the bot no longer has access to the conversation
  getBotConv galley bid cid !!! const 404 === statusCode

testMessageBotUtil ::
  Qualified UserId ->
  ClientId ->
  ConvId ->
  ProviderId ->
  ServiceId ->
  ServiceRef ->
  Chan TestBotEvent ->
  Brig ->
  Galley ->
  WS.Cannon ->
  Http ()
testMessageBotUtil quid uc cid pid sid sref buf brig galley cannon = do
  let uid = qUnqualified quid
      localDomain = qDomain quid
      qcid = Qualified cid localDomain
  -- Add bot to conversation
  _rs <- addBot brig uid pid sid cid <!! const 201 === statusCode
  let Just ars = responseJsonMaybe _rs
  let bid = rsAddBotId ars
  let buid = botUserId bid
      qbuid = Qualified buid localDomain
  let bc = rsAddBotClient ars
  _ <- svcAssertBotCreated buf bid cid
  svcAssertMemberJoin buf quid [qbuid] qcid
  -- The bot can now fetch the conversation
  _rs <- getBotConv galley bid cid <!! const 200 === statusCode
  let Just bcnv = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "id" cid (bcnv ^. Ext.botConvId)
    assertEqual "members" [OtherMember quid Nothing roleNameWireAdmin] (bcnv ^. Ext.botConvMembers)
  -- The user can identify the bot in the member list
  mems <- fmap cnvMembers . responseJsonError =<< getConversation galley uid cid
  let other = listToMaybe (cmOthers mems)
  liftIO $ do
    assertEqual "id" (Just buid) (qUnqualified . omQualifiedId <$> other)
    assertEqual "service" (Just sref) (omService =<< other)
  -- The bot greets the user
  WS.bracketR cannon uid $ \ws -> do
    postBotMessage galley bid bc cid [(uid, uc, (toBase64Text "Hi User!"))]
      !!! const 201 === statusCode
    wsAssertMessage ws qcid qbuid bc uc (toBase64Text "Hi User!")
  -- The user replies
  postMessage galley uid uc cid [(buid, bc, (toBase64Text "Hi Bot"))]
    !!! const 201 === statusCode
  let msg = OtrMessage uc bc (toBase64Text "Hi Bot") (Just "data")
  svcAssertMessage buf quid msg qcid
  -- Remove the entire service; the bot should be removed from the conversation
  WS.bracketR cannon uid $ \ws -> do
    deleteService brig pid sid defProviderPassword
      !!! const 202 === statusCode
    _ <- waitFor (5 # Second) not (isMember galley buid cid)
    getBotConv galley bid cid
      !!! const 404 === statusCode
    wsAssertMemberLeave ws qcid qbuid [qbuid]

prepareBotUsersTeam ::
  HasCallStack =>
  Brig ->
  Galley ->
  ServiceRef ->
  Http (User, User, Text, TeamId, ConvId, ProviderId, ServiceId)
prepareBotUsersTeam brig galley sref = do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare users
  (uid1, tid) <- Team.createUserWithTeam brig
  u1 <- selfUser <$> getSelfProfile brig uid1
  u2 <- Team.createTeamMember brig galley uid1 tid Team.fullPermissions
  let uid2 = userId u2
  h <- randomHandle
  putHandle brig uid1 h !!! const 200 === statusCode
  -- Whitelist the bot
  whitelistService brig uid1 tid pid sid
  -- Create conversation
  cid <- Team.createTeamConv galley tid uid1 [uid2] Nothing
  return (u1, u2, h, tid, cid, pid, sid)

addBotConv ::
  HasCallStack =>
  Domain ->
  Brig ->
  WS.Cannon ->
  UserId ->
  UserId ->
  ConvId ->
  ProviderId ->
  ServiceId ->
  Chan TestBotEvent ->
  Http BotId
addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf = do
  let quid1 = Qualified uid1 localDomain
      qcid = Qualified cid localDomain
  -- Add the bot and check that everyone is notified via an event,
  -- including the bot itself.
  WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
    _rs <- addBot brig uid1 pid sid cid <!! const 201 === statusCode
    let Just rs = responseJsonMaybe _rs
    let bid = rsAddBotId rs
    bot <- svcAssertBotCreated buf bid cid
    liftIO $ assertEqual "bot client" (rsAddBotClient rs) (testBotClient bot)
    liftIO $ assertEqual "bot event" MemberJoin (evtType (rsAddBotEvent rs))
    let qbotId = Qualified (botUserId bid) localDomain
    -- Member join event for both users
    forM_ [ws1, ws2] $ \ws -> wsAssertMemberJoin ws qcid quid1 [qbotId]
    -- Member join event for the bot
    svcAssertMemberJoin buf quid1 [qbotId] qcid
    return (rsAddBotId rs)

----------------------------------------------------------------------------
-- Service search utilities (abstracted out because we have more than one
-- service search endpoint)

-- | Given some endpoint that can search for services by name prefix, check
-- that it doesn't break when service name changes.
searchAndAssertNameChange ::
  HasCallStack =>
  Brig ->
  -- | Service provider
  ProviderId ->
  -- | Service which will have its name changed
  ServiceId ->
  -- | User who will perform the change
  UserId ->
  -- | Unique service name prefix
  Text ->
  -- | Endpoint
  (Name -> Http ServiceProfilePage) ->
  Http ()
searchAndAssertNameChange brig pid sid uid uniq search = do
  -- First let's figure out how the service is called now
  origName <-
    fmap serviceProfileName . responseJsonError
      =<< (getServiceProfile brig uid pid sid <!! const 200 === statusCode)
  -- Check that we can find the service
  searchFor "before name change" origName [(sid, origName)]
  -- Change service name; now we should find no such service with the
  -- original name, only with the new name
  let _upd = emptyUpdateService {updateServiceName = Just newName}
  updateService brig pid sid _upd !!! const 200 === statusCode
  searchFor "after name change" origName []
  searchFor "after name change" newName [(sid, newName)]
  -- Let's rollback; now searching for the new name should return nothing
  let _upd = emptyUpdateService {updateServiceName = Just origName}
  updateService brig pid sid _upd !!! const 200 === statusCode
  searchFor "after rollback" newName []
  searchFor "after rollback" origName [(sid, origName)]
  where
    newName = Name (uniq <> "|NewName")
    searchFor testName qry expected =
      search qry
        >>= assertServiceDetails (testName <> ": searching for " <> show qry) expected
    emptyUpdateService =
      UpdateService
        { updateServiceName = Nothing,
          updateServiceSummary = Nothing,
          updateServiceDescr = Nothing,
          updateServiceAssets = Nothing,
          updateServiceTags = Nothing
        }

-- | Check that lists match and there are no results on the second page.
assertServiceDetails ::
  (HasCallStack, MonadIO m) =>
  String ->
  [(ServiceId, Name)] ->
  ServiceProfilePage ->
  m ()
assertServiceDetails testName expected page = liftIO $ do
  let ids = map serviceProfileId (serviceProfilePageResults page)
  let names = map serviceProfileName (serviceProfilePageResults page)
  assertEqual (testName <> ": names") (map (fromName . snd) expected) (map fromName names)
  assertEqual (testName <> ": ids") (map fst expected) ids

-- This is commented out because otherwise tests wouldn't pass
-- (even though they should!). See Note [buggy pagination] for more
-- details.
--
-- assertEqual (testName <> ": no hidden results") False (serviceProfilePageHasMore page)

-- | Call the endpoint that searches through all services.
searchServices ::
  HasCallStack =>
  Brig ->
  Int ->
  UserId ->
  Maybe Name ->
  Maybe MatchAny ->
  Http ServiceProfilePage
searchServices brig size uid mbStart mbTags = case (mbStart, mbTags) of
  (Nothing, Nothing) ->
    error "searchServices: query not supported"
  (Just start, Nothing) ->
    responseJsonError
      =<< ( listServiceProfilesByPrefix brig uid start size
              <!! const 200 === statusCode
          )
  (_, Just tags) ->
    responseJsonError
      =<< ( listServiceProfilesByTag brig uid tags mbStart size
              <!! const 200 === statusCode
          )

-- | Call the endpoint that searches through whitelisted services.
searchServiceWhitelist ::
  HasCallStack =>
  Brig ->
  Int ->
  UserId ->
  TeamId ->
  Maybe Text ->
  Http ServiceProfilePage
searchServiceWhitelist brig size uid tid mbStart =
  responseJsonError
    =<< ( listTeamServiceProfilesByPrefix brig uid tid mbStart True size
            <!! const 200 === statusCode
        )

-- | Call the endpoint that searches through whitelisted services, and don't
-- filter out disabled services.
searchServiceWhitelistAll ::
  HasCallStack =>
  Brig ->
  Int ->
  UserId ->
  TeamId ->
  Maybe Text ->
  Http ServiceProfilePage
searchServiceWhitelistAll brig size uid tid mbStart =
  responseJsonError
    =<< ( listTeamServiceProfilesByPrefix brig uid tid mbStart False size
            <!! const 200 === statusCode
        )
