{-# LANGUAGE NondecreasingIndentation #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import API.Team.Util qualified as Team
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import Cassandra qualified as DB
import Control.Arrow ((&&&))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan
import Control.Concurrent.Timeout (threadDelay, timeout)
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Conversion
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Code qualified as Code
import Data.Domain
import Data.Handle (parseHandle)
import Data.HashMap.Strict qualified as HashMap
import Data.Id
import Data.Json.Util (toBase64Text)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Map qualified as Map
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Streaming.Network (bindRandomPortTCP)
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as Text
import Data.Time.Clock
import Data.Timeout (TimedOut (..), Timeout, TimeoutUnit (..), (#))
import Data.UUID qualified as UUID
import Data.ZAuth.Token qualified as ZAuth
import Imports hiding (threadDelay)
import Network.HTTP.Types.Status (status200, status201, status400)
import Network.Socket
import Network.Socket qualified as Socket
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai.Route qualified as Wai
import Network.Wai.Utilities.Error qualified as Error
import OpenSSL.PEM (writePublicKey)
import OpenSSL.RSA (generateRSAKey')
import System.IO.Temp (withSystemTempFile)
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import Util
import Web.Cookie (SetCookie (..), parseSetCookie)
import Wire.API.Asset hiding (Asset)
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Internal.Notification
import Wire.API.Provider
import Wire.API.Provider.Bot qualified as Ext
import Wire.API.Provider.External qualified as Ext
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.Team.Feature (featureNameBS)
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Permission
import Wire.API.User as User hiding (EmailUpdate, PasswordChange, mkName)
import Wire.API.User.Auth (CookieType (..))
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.VerificationCode qualified as Code
import Wire.VerificationCodeGen
import Wire.VerificationCodeStore.Cassandra qualified as VerificationCodeStore

tests :: Domain -> Config -> Manager -> DB.ClientState -> Brig -> Cannon -> Galley -> Nginz -> IO TestTree
tests dom conf p db b c g n = do
  pure $
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
              testDeWhitelistDeletedConv conf db b g c,
            test p "whitelist via nginz" $ testWhitelistNginz conf db b n
          ],
        testGroup
          "bot"
          [ test p "add-remove" $ testAddRemoveBot conf db b g c,
            test p "message" $ testMessageBot conf db b g c,
            test p "bad fingerprint" $ testBadFingerprint conf db b g c,
            test p "add bot forbidden" $ testAddBotForbidden conf db b g,
            test p "claim user prekeys" $ testClaimUserPrekeys conf db b g,
            test p "list user profiles" $ testListUserProfiles conf db b g,
            test p "get user clients" $ testGetUserClients conf db b g
          ],
        testGroup
          "bot-teams"
          [ test p "add-remove" $ testAddRemoveBotTeam conf db b g c,
            test p "add-remove-access-denied-for-non-conv-admin" $ testNonConvAdminCannotAddRemoveBot conf db b g,
            test p "team-only" $ testBotTeamOnlyConv conf db b g c,
            test p "message" $ testMessageBotTeam conf db b g c,
            test p "delete conv" $ testDeleteConvBotTeam conf db b g c,
            test p "delete team" $ testDeleteTeamBotTeam conf db b g c
          ],
        testGroup
          "block bot api if 2nd factor password challenge enabled"
          [ test p "add" $ testAddBotBlocked conf db b g,
            test p "GET /bot/conversation (galley endpoint)" $ testGetBotConvBlocked conf db b g c
          ]
      ]

----------------------------------------------------------------------------
-- Config

data Config = Config
  { privateKey :: FilePath,
    publicKey :: FilePath,
    cert :: FilePath,
    botHost :: Text
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
  response <- retryWhileN 10 ((==) 429 . statusCode) $ registerProvider brig new
  liftIO $ statusCode response @?= 201

testPasswordResetProvider :: DB.ClientState -> Brig -> Http ()
testPasswordResetProvider db brig = do
  prv <- randomProvider db brig
  let email = providerEmail prv
  let newPw = plainTextPassword6Unsafe "newsupersecret"
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
    resetPw :: PlainTextPassword6 -> Email -> Http ResponseLBS
    resetPw newPw email = do
      -- Get the code directly from the DB
      let gen = mkVerificationCodeGen email
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
  let genOrig = mkVerificationCodeGen origEmail
  Just vcodePw <- lookupCode db genOrig Code.PasswordReset
  let passwordResetData =
        CompletePasswordReset
          (Code.codeKey vcodePw)
          (Code.codeValue vcodePw)
          (plainTextPassword6Unsafe "doesnotmatter")
  -- Activate the new email
  let genNew = mkVerificationCodeGen newEmail
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
  let newPass = plainTextPassword6Unsafe "newpass"
  let pwChangeFail = PasswordChange (plainTextPassword6Unsafe "notcorrect") newPass
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
  services :: [Service] <- responseJsonError =<< getServices brig pid <!! const 200 === statusCode
  liftIO $ do
    assertBool "list of all services should not be empty" (not (null services))
  providerServices :: [ServiceProfile] <- responseJsonError =<< getProviderServices brig uid pid <!! const 200 === statusCode
  liftIO $ do
    assertBool "list of provider services should not be empty" (not (null providerServices))

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
  let search :: (HasCallStack) => Name -> Http ServiceProfilePage
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
        pure r1
  -- This function searches for a prefix and check that the results match
  -- our known list of services
  let searchAndCheck :: (HasCallStack) => Name -> Http [ServiceProfile]
      searchAndCheck name = do
        result <- search name
        assertServiceDetails ("name " <> show name) (select name services) result
        pure (serviceProfilePageResults result)
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
  let uid2 = userId u2
      Qualified uid1 localDomain = userQualifiedId u1
      luid1 = toLocalUnsafe localDomain uid1
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  cnv <- responseJsonError =<< (createConv galley uid1 [uid2] <!! const 201 === statusCode)
  let (cid, qcid) = (qUnqualified &&& id) (cnvQualifiedId cnv)
  -- Add two bots there
  bid1 <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  bid2 <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  liftIO $ assertBool "bot ids should be different" (bid1 /= bid2)
  let lbuid1 = qualifyAs luid1 . botUserId $ bid1
      lbuid2 = qualifyAs luid1 . botUserId $ bid2
  -- Delete the service; the bots should be removed from the conversation
  WS.bracketR cannon uid1 $ \ws -> do
    deleteService brig pid sid defProviderPassword
      !!! const 202 === statusCode
    _ <- waitFor (5 # Second) not (isMember galley lbuid1 cid)
    _ <- waitFor (5 # Second) not (isMember galley lbuid2 cid)
    void $ aFewTimes 12 (getBotConv galley bid1 cid) ((== 404) . statusCode)
    void $ aFewTimes 12 (getBotConv galley bid2 cid) ((== 404) . statusCode)
    wsAssertMemberLeave ws qcid (tUntagged lbuid1) [tUntagged lbuid1]
    wsAssertMemberLeave ws qcid (tUntagged lbuid2) [tUntagged lbuid2]
  -- The service should not be available
  void $ aFewTimes 12 (getService brig pid sid) ((== 404) . statusCode)
  void $ aFewTimes 12 (getServiceProfile brig uid1 pid sid) ((== 404) . statusCode)

testAddRemoveBot :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBot config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (pid, sid, u1, u2, h) <- prepareUsers sref brig
  let uid1 = userId u1
      quid1 = userQualifiedId u1
      localDomain = qDomain quid1
      uid2 = userId u2
  -- Create conversation
  _rs <- createConv galley uid1 [uid2] <!! const 201 === statusCode
  let Just cnv = responseJsonMaybe _rs
  let cid = qUnqualified . cnvQualifiedId $ cnv
  testAddRemoveBotUtil localDomain pid sid cid u1 u2 h sref buf brig galley cannon

testAddBotForbidden :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testAddBotForbidden config db brig galley = withTestService config db brig defServiceApp $ \sref _ -> do
  (pid, sid, userId -> uid1, userId -> uid2, _) <- prepareUsers sref brig
  -- Create conversation without the service access role
  let accessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole]
  _rs <- createConvWithAccessRoles (Just accessRoles) galley uid1 [uid2] <!! const 201 === statusCode
  let Just cnv = responseJsonMaybe _rs
  let cid = qUnqualified . cnvQualifiedId $ cnv
  addBot brig uid1 pid sid cid !!! do
    const 403 === statusCode
    const (Just "invalid-conversation") === fmap Error.label . responseJsonMaybe

testClaimUserPrekeys :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testClaimUserPrekeys config db brig galley = withTestService config db brig defServiceApp $ \sref _ -> do
  (pid, sid, u1, _u2, _h) <- prepareUsers sref brig
  cid <- do
    rs <- createConv galley (User.userId u1) [] <!! const 201 === statusCode
    let Just cnv = responseJsonMaybe rs
    let cid = qUnqualified . cnvQualifiedId $ cnv
    pure cid
  addBotResponse :: AddBotResponse <- responseJsonError =<< addBot brig (User.userId u1) pid sid cid <!! const 201 === statusCode
  let bid = addBotResponse.rsAddBotId
  let new = defNewClient TemporaryClientType (take 1 somePrekeys) (Imports.head someLastPrekeys)
  c :: Client <- responseJsonError =<< addClient brig (User.userId u1) new

  let userClients = UserClients $ Map.fromList [((User.userId u1), Set.fromList [c.clientId])]
  actual <- responseJsonError =<< claimUsersPrekeys brig bid userClients <!! const 200 === statusCode

  let expected =
        UserClientPrekeyMap $
          UserClientMap $
            Map.fromList [((User.userId u1), Map.fromList [(c.clientId, Just (Imports.head somePrekeys))])]

  liftIO $ assertEqual "claim prekeys" expected actual

testListUserProfiles :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testListUserProfiles config db brig galley = withTestService config db brig defServiceApp $ \sref _ -> do
  (pid, sid, u1, u2, _h) <- prepareUsers sref brig
  cid <- do
    rs <- createConv galley (User.userId u1) [] <!! const 201 === statusCode
    let Just cnv = responseJsonMaybe rs
    let cid = qUnqualified . cnvQualifiedId $ cnv
    pure cid
  addBotResponse :: AddBotResponse <- responseJsonError =<< addBot brig (User.userId u1) pid sid cid <!! const 201 === statusCode
  let bid = addBotResponse.rsAddBotId
  resp :: [Ext.BotUserView] <- responseJsonError =<< listUserProfiles brig bid [(User.userId u1), (User.userId u2)] <!! const 200 === statusCode
  liftIO $ Set.fromList (fmap (.botUserViewId) resp) @?= Set.fromList [(User.userId u1), (User.userId u2)]

testGetUserClients :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testGetUserClients config db brig galley = withTestService config db brig defServiceApp $ \sref _ -> do
  (pid, sid, u1, _u2, _h) <- prepareUsers sref brig
  cid <- do
    rs <- createConv galley (User.userId u1) [] <!! const 201 === statusCode
    let Just cnv = responseJsonMaybe rs
    let cid = qUnqualified . cnvQualifiedId $ cnv
    pure cid
  addBotResponse :: AddBotResponse <- responseJsonError =<< addBot brig (User.userId u1) pid sid cid <!! const 201 === statusCode
  let bid = addBotResponse.rsAddBotId
  let new = defNewClient TemporaryClientType (take 1 somePrekeys) (Imports.head someLastPrekeys)
  expected :: Client <- responseJsonError =<< addClient brig (User.userId u1) new
  [actual] :: [PubClient] <- responseJsonError =<< getUserClients brig bid (User.userId u1) <!! const 200 === statusCode
  liftIO $ actual.pubClientId @?= expected.clientId

testAddBotBlocked :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testAddBotBlocked config db brig galley = withTestService config db brig defServiceApp $ \sref _buf -> do
  (userId -> u1, _, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  enabled2ndFaForTeamInternal galley tid
  addBot brig u1 pid sid cid !!! do
    const 403 === statusCode
    const (Just "access-denied") === fmap Error.label . responseJsonMaybe

testNonConvAdminCannotAddRemoveBot :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testNonConvAdminCannotAddRemoveBot config db brig galley = withTestService config db brig defServiceApp $ \sref _buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  (ownerId, tid) <- Team.createUserWithTeam brig
  member <- Team.createTeamMember brig galley ownerId tid fullPermissions
  let memberId = userId member
  whitelistService brig ownerId tid pid sid
  cid <- Team.createTeamConvWithRole roleNameWireMember galley tid ownerId [memberId] Nothing
  addBot brig memberId pid sid cid !!! do
    const 403 === statusCode
    const (Just "access-denied") === fmap Error.label . responseJsonMaybe
  rs <- responseJsonError =<< addBot brig ownerId pid sid cid <!! const 201 === statusCode
  let bid = rsAddBotId rs
      buid = botUserId bid
  getUser brig ownerId buid !!! const 200 === statusCode
  removeBot brig memberId cid bid !!! do
    const 403 === statusCode
    const (Just "access-denied") === fmap Error.label . responseJsonMaybe
  -- also check the internal galley API
  removeBotInternal galley memberId cid bid !!! do
    const 403 === statusCode
    const (Just "action-denied") === fmap Error.label . responseJsonMaybe

testGetBotConvBlocked :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testGetBotConvBlocked config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (user1, userId -> u2, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let Qualified u1 localDomain = userQualifiedId user1
  bid <- addBotConv localDomain brig cannon u1 u2 cid pid sid buf
  getBotConv galley bid cid !!! const 200 === statusCode
  enabled2ndFaForTeamInternal galley tid
  getBotConv galley bid cid !!! do
    const 403 === statusCode
    const (Just "access-denied") === fmap Error.label . responseJsonMaybe

prepareUsers :: ServiceRef -> Brig -> Http (ProviderId, ServiceId, User, User, Text)
prepareUsers sref brig = do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare users
  u1 <- createUser "Ernie" brig
  u2 <- createUser "Bert" brig
  let uid1 = userId u1
      uid2 = userId u2
  h <- randomHandle
  putHandle brig uid1 h !!! const 200 === statusCode
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  pure (pid, sid, u1, u2, h)

testMessageBot :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBot config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare user with client
  usr <- createUser "User" brig
  let uid = userId usr
  let quid = userQualifiedId usr
  let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
  _rs <- addClient brig uid new <!! const 201 === statusCode
  let Just uc = clientId <$> responseJsonMaybe _rs
  -- Create conversation
  _rs <- createConv galley uid [] <!! const 201 === statusCode
  let Just cid = qUnqualified . cnvQualifiedId <$> responseJsonMaybe _rs
  testMessageBotUtil quid uc cid pid sid sref buf brig galley cannon

testBadFingerprint :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testBadFingerprint config db brig galley _cannon = withFreePortAnyAddr $ \(sPort, sock) -> do
  -- Generate a random key and register a service using that key
  sref <- withSystemTempFile "wire-provider.key" $ \fp h -> do
    ServiceKeyPEM key <- randServiceKey
    liftIO $ BS.hPut h (pemWriteBS key) >> hClose h
    registerService config {publicKey = fp} sPort db brig
  -- Run the service with a different key (i.e. the key from the config)
  runService config sPort sock defServiceApp $ \_ -> do
    let pid = sref ^. serviceRefProvider
    let sid = sref ^. serviceRefId
    -- Prepare user with client
    usr <- createUser "User" brig
    let uid = userId usr
    let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
    _rs <- addClient brig uid new <!! const 201 === statusCode
    -- Create conversation
    _rs <- createConv galley uid [] <!! const 201 === statusCode
    let Just cid = qUnqualified . cnvQualifiedId <$> responseJsonMaybe _rs
    -- Try to add a bot and observe failure
    addBot brig uid pid sid cid
      !!! const 502 === statusCode

testAddRemoveBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (u1, u2, h, _, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let quid1 = userQualifiedId u1
      localDomain = qDomain quid1
  testAddRemoveBotUtil localDomain pid sid cid u1 u2 h sref buf brig galley cannon

testBotTeamOnlyConv :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testBotTeamOnlyConv config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  (u1, u2, _h, _tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
  let uid2 = userId u2
      Qualified uid1 localDomain = userQualifiedId u1
      luid1 = toLocalUnsafe localDomain uid1
      qcid = Qualified cid localDomain
  -- Make the conversation team-only and check that the bot can't be added
  -- to the conversation
  setAccessRole uid1 qcid (Set.fromList [TeamMemberAccessRole])
  addBot brig uid1 pid sid cid !!! do
    const 403 === statusCode
    const (Just "invalid-conversation") === fmap Error.label . responseJsonMaybe
  -- Make the conversation allowed for guests and add the bot successfully
  setAccessRole uid1 qcid (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  bid <- addBotConv localDomain brig cannon uid1 uid2 cid pid sid buf
  let lbuid = qualifyAs luid1 . botUserId $ bid
  -- Make the conversation team-only again and check that the bot has been removed
  WS.bracketR cannon uid1 $ \ws -> do
    setAccessRole uid1 qcid (Set.fromList [TeamMemberAccessRole])
    _ <- waitFor (5 # Second) not (isMember galley lbuid cid)
    getBotConv galley bid cid
      !!! const 404 === statusCode
    -- Two events are sent concurrently:
    -- - ConvAccessUpdate
    -- - MemberLeave (for the bot)
    --
    -- We cannot guarantee the order, so we have to check for both
    let expectedConvAccessData = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole])
        expectedMemberLeave = [tUntagged lbuid]
        assertAndRetrieveEvent = do
          event <-
            timeout (5 # Second) (readChan buf)
              >>= assertJust
              >>= assertBotMessage
          assertAccessUpdateOrMemberLeave (tUntagged luid1) expectedConvAccessData (tUntagged lbuid) expectedMemberLeave qcid event
          pure event
    event1 <- assertAndRetrieveEvent
    event2 <- assertAndRetrieveEvent
    -- Ensure there is exactly one of each types of event
    liftIO $
      assertEqual
        "there should be 1 ConvAccessUpdate and 1 MemberLeave event"
        (Set.fromList [ConvAccessUpdate, MemberLeave])
        (Set.fromList (map evtType [event1, event2]))
    wsAssertMemberLeave ws qcid (tUntagged lbuid) [tUntagged lbuid]
  where
    assertBotMessage :: (HasCallStack, MonadIO m) => TestBotEvent -> m Event
    assertBotMessage =
      liftIO . \case
        TestBotMessage e -> pure e
        evt -> assertFailure $ "expected TestBotMessage, got: " <> show evt
    assertAccessUpdateOrMemberLeave :: (HasCallStack, MonadIO m) => Qualified UserId -> ConversationAccessData -> Qualified UserId -> [Qualified UserId] -> Qualified ConvId -> Event -> m ()
    assertAccessUpdateOrMemberLeave updFrom upd leaveFrom gone cnv e = liftIO $
      case evtType e of
        ConvAccessUpdate -> do
          assertEqual "conv" cnv (evtConv e)
          assertEqual "user" updFrom (evtFrom e)
          assertEqual "event data" (EdConvAccessUpdate upd) (evtData e)
        MemberLeave -> do
          let msg = QualifiedUserIdList gone
          assertEqual "conv" cnv (evtConv e)
          assertEqual "user" leaveFrom (evtFrom e)
          assertEqual "event data" (EdMembersLeave EdReasonRemoved msg) (evtData e)
        _ ->
          assertFailure $ "expected event of type: ConvAccessUpdate or MemberLeave, got: " <> show e
    setAccessRole uid qcid role =
      updateConversationAccess galley uid qcid [InviteAccess] role
        !!! const 200 === statusCode

testMessageBotTeam :: Config -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBotTeam config db brig galley cannon = withTestService config db brig defServiceApp $ \sref buf -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  -- Prepare user with client
  (uid, tid) <- Team.createUserWithTeam brig
  let new = defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)
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
    getConversationQualified galley uid qcid !!! const 404 === statusCode
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
    void $ retryWhileN 20 (/= User.Deleted) (getStatus brig uid)
    chkStatus brig uid User.Deleted
    aFewTimes 11 (getConversationQualified galley uid qcid) ((== 404) . statusCode)
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
  member <- userId <$> Team.createTeamMember brig galley owner tid noPermissions
  listTeamServiceProfilesByPrefix brig member tid Nothing True 20
    !!! const 200 === statusCode

testWhitelistUpdatePermissions :: Config -> DB.ClientState -> Brig -> Galley -> Http ()
testWhitelistUpdatePermissions config db brig galley = do
  -- Create a team
  (owner, tid) <- Team.createUserWithTeam brig
  -- Create a team admin
  let Just adminPermissions = newPermissions serviceWhitelistPermissions mempty
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
  _uid <- userId <$> Team.createTeamMember brig galley owner tid noPermissions
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
  uid <- userId <$> Team.createTeamMember brig galley owner tid noPermissions
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
  let search :: (HasCallStack) => Maybe Text -> Http ServiceProfilePage
      search mbName = do
        r1 <- searchServiceWhitelist brig 20 uid tid mbName
        r2 <- searchServiceWhitelistAll brig 20 uid tid mbName
        liftIO $
          assertEqual
            ("search for " <> show mbName <> " with and without filtering")
            r1
            r2
        pure r1
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
  let searchAndCheck :: (HasCallStack) => Name -> Http [ServiceProfile]
      searchAndCheck (Name name) = do
        result <- search (Just name)
        assertServiceDetails ("name " <> show name) (select name services) result
        pure (serviceProfilePageResults result)
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
      lowner = toLocalUnsafe localDomain owner
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
        lbuid = qualifyAs lowner . botUserId $ bid
    _ <- svcAssertBotCreated buf bid cid
    svcAssertMemberJoin buf qowner [tUntagged lbuid] qcid
    -- De-whitelist the service; both bots should be kicked out
    WS.bracketR cannon owner $ \ws -> do
      dewhitelistService brig owner tid pid sid
      _ <- waitFor (2 # Second) not (isMember galley lbuid cid)
      getBotConv galley bid cid
        !!! const 404 === statusCode
      wsAssertMemberLeave ws qcid qowner [tUntagged lbuid]
      svcAssertMemberLeave buf qowner [tUntagged lbuid] qcid
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
  PlainTextPassword6 ->
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
  PlainTextPassword6 ->
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

getServices :: Brig -> ProviderId -> Http ResponseLBS
getServices brig pid =
  get $
    brig
      . path "/provider/services"
      . header "Z-Type" "provider"
      . header "Z-Provider" (toByteString' pid)

getProviderServices :: Brig -> UserId -> ProviderId -> Http ResponseLBS
getProviderServices brig uid pid =
  get $
    brig
      . paths ["providers", toByteString' pid, "services"]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)

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
  PlainTextPassword6 ->
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

data RemoveBot = RemoveBot
  { _rmBotConv :: !ConvId,
    _rmBotId :: !BotId
  }

instance ToJSON RemoveBot where
  toJSON a =
    object
      [ "conversation" .= _rmBotConv a,
        "bot" .= _rmBotId a
      ]

removeBotInternal ::
  Galley ->
  UserId ->
  ConvId ->
  BotId ->
  Http ResponseLBS
removeBotInternal galley uid cid bid =
  delete $
    galley
      . paths ["i", "bots"]
      . header "Z-User" (toByteString' uid)
      . Bilge.json (RemoveBot cid bid)

createConv ::
  Galley ->
  UserId ->
  [UserId] ->
  Http ResponseLBS
createConv = createConvWithAccessRoles Nothing

createConvWithAccessRoles ::
  Maybe (Set AccessRole) ->
  Galley ->
  UserId ->
  [UserId] ->
  Http ResponseLBS
createConvWithAccessRoles ars g u us =
  post $
    g
      . path "/conversations"
      . header "Z-User" (toByteString' u)
      . header "Z-Type" "access"
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode conv))
  where
    conv = NewConv us [] Nothing Set.empty ars Nothing Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag

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
  Qualified ConvId ->
  [Access] ->
  Set AccessRole ->
  Http ResponseLBS
updateConversationAccess galley uid qcid access role =
  put $
    galley
      . paths
        [ "conversations",
          toByteString' (qDomain qcid),
          toByteString' (qUnqualified qcid),
          "access"
        ]
      . header "Z-Type" "access"
      . header "Z-User" (toByteString' uid)
      . header "Z-Connection" "conn"
      . contentJson
      . body (RequestBodyLBS (encode upd))
  where
    upd = ConversationAccessData (Set.fromList access) role

enabled2ndFaForTeamInternal :: Galley -> TeamId -> Http ()
enabled2ndFaForTeamInternal galley tid = do
  put
    ( galley
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig, toByteString' Public.LockStatusUnlocked]
        . contentJson
    )
    !!! const 200 === statusCode
  put
    ( galley
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @Public.SndFactorPasswordChallengeConfig]
        . contentJson
        . Bilge.json (Public.Feature Public.FeatureStatusEnabled Public.SndFactorPasswordChallengeConfig)
    )
    !!! const 200 === statusCode

getBotSelf :: Brig -> BotId -> Http ResponseLBS
getBotSelf brig bid =
  get $
    brig
      . path "/bot/self"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)

getBotClient :: Brig -> BotId -> Http ResponseLBS
getBotClient brig bid =
  get $
    brig
      . path "/bot/client"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)
      . contentJson

getBotPreKeyIds :: Brig -> BotId -> Http ResponseLBS
getBotPreKeyIds brig bid =
  get $
    brig
      . path "/bot/client/prekeys"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)

updateBotPrekeys :: Brig -> BotId -> [Prekey] -> Http ResponseLBS
updateBotPrekeys brig bid prekeys =
  post $
    brig
      . path "/bot/client/prekeys"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)
      . contentJson
      . body (RequestBodyLBS (encode (UpdateBotPrekeys prekeys)))

claimUsersPrekeys :: Brig -> BotId -> UserClients -> Http ResponseLBS
claimUsersPrekeys brig bid ucs =
  post $
    brig
      . path "/bot/users/prekeys"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)
      . contentJson
      . body (RequestBodyLBS (encode ucs))

listUserProfiles :: Brig -> BotId -> [UserId] -> Http ResponseLBS
listUserProfiles brig bid uids =
  get $
    brig
      . path "/bot/users"
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)
      . queryItem "ids" (C8.intercalate "," $ toByteString' <$> uids)

getUserClients :: Brig -> BotId -> UserId -> Http ResponseLBS
getUserClients brig bid uid =
  get $
    brig
      . paths ["bot", "users", toByteString' uid, "clients"]
      . header "Z-Type" "bot"
      . header "Z-Bot" (toByteString' bid)

--------------------------------------------------------------------------------
-- DB Operations

lookupCode :: (MonadIO m) => DB.ClientState -> VerificationCodeGen -> Code.Scope -> m (Maybe Code.Code)
lookupCode db gen = liftIO . DB.runClient db . VerificationCodeStore.lookupCodeImpl gen.genKey

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
      let gen = mkVerificationCodeGen email
      Just vcode <- lookupCode db gen Code.IdentityVerification
      activateProvider brig (Code.codeKey vcode) (Code.codeValue vcode)
        !!! const 200 === statusCode
    Nothing -> do
      rs <-
        getProviderActivationCodeInternal brig email
          <!! const 200 === statusCode
      let Just pair = responseJsonMaybe rs :: Maybe Code.KeyValuePair
      activateProvider brig (Code.key pair) (Code.code pair)
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

randomProvider :: (HasCallStack) => DB.ClientState -> Brig -> Http Provider
randomProvider db brig = do
  email <- randomEmail
  let gen = mkVerificationCodeGen email
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
  pure prv

addGetService :: (HasCallStack) => Brig -> ProviderId -> NewService -> Http Service
addGetService brig pid new = do
  _rs <- addService brig pid new <!! const 201 === statusCode
  let Just srs = responseJsonMaybe _rs
  let sid = rsNewServiceId srs
  _rs <- getService brig pid sid <!! const 200 === statusCode
  let Just svc = responseJsonMaybe _rs
  pure svc

enableService :: (HasCallStack) => Brig -> ProviderId -> ServiceId -> Http ()
enableService brig pid sid = do
  let upd =
        (mkUpdateServiceConn defProviderPassword)
          { updateServiceConnEnabled = Just True
          }
  updateServiceConn brig pid sid upd
    !!! const 200 === statusCode

disableService :: (HasCallStack) => Brig -> ProviderId -> ServiceId -> Http ()
disableService brig pid sid = do
  let upd =
        (mkUpdateServiceConn defProviderPassword)
          { updateServiceConnEnabled = Just False
          }
  updateServiceConn brig pid sid upd
    !!! const 200 === statusCode

whitelistServiceNginz ::
  (HasCallStack) =>
  Nginz ->
  -- | Team owner
  User ->
  -- | Team
  TeamId ->
  ProviderId ->
  ServiceId ->
  Http ()
whitelistServiceNginz nginz user tid pid sid =
  updateServiceWhitelistNginz nginz user tid (UpdateServiceWhitelist pid sid True) !!! const 200 === statusCode

updateServiceWhitelistNginz ::
  Nginz ->
  User ->
  TeamId ->
  UpdateServiceWhitelist ->
  Http ResponseLBS
updateServiceWhitelistNginz nginz user tid upd = do
  let Just email = userEmail user
  rs <- login nginz (defEmailLogin email) PersistentCookie <!! const 200 === statusCode
  let t = decodeToken rs
  post $
    nginz
      . paths ["teams", toByteString' tid, "services", "whitelist"]
      . header "Authorization" ("Bearer " <> toByteString' t)
      . contentJson
      . body (RequestBodyLBS (encode upd))

whitelistService ::
  (HasCallStack) =>
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
  (HasCallStack) =>
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

defNewService :: (MonadIO m) => Config -> m NewService
defNewService config = liftIO $ do
  key <- readServiceKey (publicKey config)
  pure
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

defProviderPassword :: PlainTextPassword6
defProviderPassword = plainTextPassword6Unsafe "password"

defServiceName :: Name
defServiceName = Name "Test Service"

defServiceDescr :: Text
defServiceDescr = "Test service description"

defServiceUrl :: HttpsUrl
defServiceUrl = fromJust (fromByteString "https://localhost/test")

defServiceTags :: Range 1 3 (Set ServiceTag)
defServiceTags = unsafeRange (Set.singleton SocialTag)

defServiceAssets :: [Asset]
defServiceAssets =
  [ ImageAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring)
      (Just AssetComplete)
  ]

-- TODO: defServiceToken :: ServiceToken

readServiceKey :: (MonadIO m) => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
  bs <- BS.readFile fp
  let Right [k] = pemParseBS bs
  pure (ServiceKeyPEM k)

randServiceKey :: (MonadIO m) => m ServiceKeyPEM
randServiceKey = liftIO $ do
  kp <- generateRSAKey' 4096 65537
  Right [k] <- pemParseBS . C8.pack <$> writePublicKey kp
  pure (ServiceKeyPEM k)

waitFor :: (MonadIO m) => Timeout -> (a -> Bool) -> m a -> m a
waitFor t f ma = do
  a <- ma
  if
    | f a -> pure a
    | t <= 0 -> liftIO $ throwM TimedOut
    | otherwise -> do
        liftIO $ threadDelay (1 # Second)
        waitFor (t - 1 # Second) f ma

withFreePortAnyAddr :: (MonadMask m, MonadIO m) => ((Warp.Port, Socket) -> m a) -> m a
withFreePortAnyAddr = bracket openFreePortAnyAddr (liftIO . Socket.close . snd)

openFreePortAnyAddr :: (MonadIO m) => m (Warp.Port, Socket)
openFreePortAnyAddr = liftIO $ bindRandomPortTCP "*"

-- | Run a test case with an external service application.
withTestService ::
  Config ->
  DB.ClientState ->
  Brig ->
  (Chan e -> Application) ->
  (ServiceRef -> Chan e -> Http a) ->
  Http a
withTestService config db brig mkApp go = withFreePortAnyAddr $ \(sPort, sock) -> do
  sref <- registerService config sPort db brig
  runService config sPort sock mkApp (go sref)

registerService :: Config -> Warp.Port -> DB.ClientState -> Brig -> Http ServiceRef
registerService config sPort db brig = do
  prv <- randomProvider db brig
  new <- defNewService config
  let Just url =
        fromByteString $
          encodeUtf8 (botHost config)
            <> ":"
            <> C8.pack (show sPort)
  svc <- addGetService brig (providerId prv) (new {newServiceUrl = url})
  let pid = providerId prv
  let sid = serviceId svc
  enableService brig pid sid
  pure (newServiceRef sid pid)

runService ::
  Config ->
  Warp.Port ->
  Socket ->
  (Chan e -> Application) ->
  (Chan e -> Http a) ->
  Http a
runService config sPort sock mkApp go = do
  let tlss = Warp.tlsSettings (cert config) (privateKey config)
  let defs = Warp.defaultSettings {Warp.settingsPort = sPort}
  buf <- liftIO newChan
  srv <-
    liftIO . Async.async $
      Warp.runTLSSocket tlss defs sock $
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
  deriving (Show, Eq)

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
          let pks = [head somePrekeys]
          let lpk = head someLastPrekeys
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

wsAssertMemberJoin :: (HasCallStack, MonadIO m) => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> m ()
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

wsAssertMemberLeave :: (HasCallStack, MonadIO m) => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> m ()
wsAssertMemberLeave ws conv usr old = void $
  liftIO $
    WS.assertMatch (5 # Second) ws $
      \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv e @?= conv
        evtType e @?= MemberLeave
        evtFrom e @?= usr
        evtData e @?= EdMembersLeave EdReasonRemoved (QualifiedUserIdList old)

wsAssertConvDelete :: (HasCallStack, MonadIO m) => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> m ()
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

wsAssertMessage :: (HasCallStack, MonadIO m) => WS.WebSocket -> Qualified ConvId -> Qualified UserId -> ClientId -> ClientId -> Text -> m ()
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

svcAssertMemberJoin :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> Qualified UserId -> [Qualified UserId] -> Qualified ConvId -> m ()
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

svcAssertMemberLeave :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> Qualified UserId -> [Qualified UserId] -> Qualified ConvId -> m ()
svcAssertMemberLeave buf usr gone cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      let msg = QualifiedUserIdList gone
      assertEqual "event type" MemberLeave (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" (EdMembersLeave EdReasonRemoved msg) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: member-leave)"

svcAssertConvDelete :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> Qualified UserId -> Qualified ConvId -> m ()
svcAssertConvDelete buf usr cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      assertEqual "event type" ConvDelete (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" usr (evtFrom e)
      assertEqual "event data" EdConvDelete (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: conv-delete)"

svcAssertBotCreated :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> BotId -> ConvId -> m TestBot
svcAssertBotCreated buf bid cid = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotCreated b) -> do
      assertEqual "bot ID" bid (testBotId b)
      assertEqual "conv" cid (testBotConv b ^. Ext.botConvId)
      -- TODO: Verify the conversation name
      -- TODO: Verify the list of members
      pure b
    _ -> assertFailure "Event timeout (TestBotCreated)"

svcAssertMessage :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> Qualified UserId -> OtrMessage -> Qualified ConvId -> m ()
svcAssertMessage buf from msg cnv = liftIO $ do
  evt <- timeout (5 # Second) $ readChan buf
  case evt of
    Just (TestBotMessage e) -> do
      assertEqual "event type" OtrMessageAdd (evtType e)
      assertEqual "conv" cnv (evtConv e)
      assertEqual "user" from (evtFrom e)
      assertEqual "event data" (EdOtrMessage msg) (evtData e)
    _ -> assertFailure "Event timeout (TestBotMessage: otr-message-add)"

svcAssertEventuallyConvDelete :: (HasCallStack, MonadIO m) => Chan TestBotEvent -> Qualified UserId -> Qualified ConvId -> m ()
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
    text :: (ToByteString a) => a -> Text
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
    getBotSelf brig bid !!! const 200 === statusCode
    (randomId >>= getBotSelf brig . BotId) !!! const 404 === statusCode
    botClient :: Client <- responseJsonError =<< getBotClient brig bid <!! const 200 === statusCode
    liftIO $ assertEqual "bot client" rs.rsAddBotClient botClient.clientId
    (randomId >>= getBotClient brig . BotId) !!! const 404 === statusCode
    bot <- svcAssertBotCreated buf bid cid
    liftIO $ assertEqual "bot client" rs.rsAddBotClient bot.testBotClient
    liftIO $ assertEqual "bot event" MemberJoin (evtType (rsAddBotEvent rs))
    -- just check that these endpoints works
    getBotPreKeyIds brig bid !!! const 200 === statusCode
    updateBotPrekeys brig bid bot.testBotPrekeys !!! const 200 === statusCode
    -- Member join event for both users
    forM_ [ws1, ws2] $ \ws -> wsAssertMemberJoin ws qcid quid1 [qbuid]
    -- Member join event for the bot
    svcAssertMemberJoin buf quid1 [qbuid] qcid
    pure (rs, bot)
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
  liftIO $ assertEqual "handle" (Just (fromJust $ parseHandle h)) u1Handle
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
  let isBotPrekey = (`elem` bot.testBotPrekeys) . prekeyData
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
  let Qualified uid localDomain = quid
      luid = toLocalUnsafe localDomain uid
      qcid = Qualified cid localDomain
  -- Add bot to conversation
  _rs <- addBot brig uid pid sid cid <!! const 201 === statusCode
  let Just ars = responseJsonMaybe _rs
  let bid = rsAddBotId ars
  let buid = botUserId bid
      lbuid = qualifyAs luid buid
  let bc = rsAddBotClient ars
  _ <- svcAssertBotCreated buf bid cid
  svcAssertMemberJoin buf quid [tUntagged lbuid] qcid
  -- The bot can now fetch the conversation
  _rs <- getBotConv galley bid cid <!! const 200 === statusCode
  let Just bcnv = responseJsonMaybe _rs
  liftIO $ do
    assertEqual "id" cid (bcnv ^. Ext.botConvId)
    assertEqual "members" [OtherMember quid Nothing roleNameWireAdmin] (bcnv ^. Ext.botConvMembers)
  -- The user can identify the bot in the member list
  mems <- fmap cnvMembers . responseJsonError =<< getConversationQualified galley uid qcid
  let other = listToMaybe (cmOthers mems)
  liftIO $ do
    assertEqual "id" (Just buid) (qUnqualified . omQualifiedId <$> other)
    assertEqual "service" (Just sref) (omService =<< other)
  -- The bot greets the user
  WS.bracketR cannon uid $ \ws -> do
    postBotMessage galley bid bc cid [(uid, uc, toBase64Text "Hi User!")]
      !!! const 201 === statusCode
    wsAssertMessage ws qcid (tUntagged lbuid) bc uc (toBase64Text "Hi User!")
  -- The user replies
  postMessage galley uid uc cid [(buid, bc, toBase64Text "Hi Bot")]
    !!! const 201 === statusCode
  let msg = OtrMessage uc bc (toBase64Text "Hi Bot") (Just "data")
  svcAssertMessage buf quid msg qcid
  -- Remove the entire service; the bot should be removed from the conversation
  WS.bracketR cannon uid $ \ws -> do
    deleteService brig pid sid defProviderPassword
      !!! const 202 === statusCode
    _ <- waitFor (5 # Second) not (isMember galley lbuid cid)
    getBotConv galley bid cid
      !!! const 404 === statusCode
    wsAssertMemberLeave ws qcid (tUntagged lbuid) [tUntagged lbuid]

prepareBotUsersTeam ::
  (HasCallStack) =>
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
  u2 <- Team.createTeamMember brig galley uid1 tid fullPermissions
  let uid2 = userId u2
  h <- randomHandle
  putHandle brig uid1 h !!! const 200 === statusCode
  -- Whitelist the bot
  whitelistService brig uid1 tid pid sid
  -- Create conversation
  cid <- Team.createTeamConv galley tid uid1 [uid2] Nothing
  pure (u1, u2, h, tid, cid, pid, sid)

testWhitelistNginz :: Config -> DB.ClientState -> Brig -> Nginz -> Http ()
testWhitelistNginz config db brig nginz = withTestService config db brig defServiceApp $ \sref _ -> do
  let pid = sref ^. serviceRefProvider
  let sid = sref ^. serviceRefId
  (admin, tid) <- Team.createUserWithTeam brig
  adminUser <- selfUser <$> getSelfProfile brig admin
  whitelistServiceNginz nginz adminUser tid pid sid

addBotConv ::
  (HasCallStack) =>
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
    pure (rsAddBotId rs)

----------------------------------------------------------------------------
-- Service search utilities (abstracted out because we have more than one
-- service search endpoint)

-- | Given some endpoint that can search for services by name prefix, check
-- that it doesn't break when service name changes.
searchAndAssertNameChange ::
  (HasCallStack) =>
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
  (HasCallStack) =>
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
  (HasCallStack) =>
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
  (HasCallStack) =>
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
