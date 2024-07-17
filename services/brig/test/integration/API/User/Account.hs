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

module API.User.Account
  ( tests,
  )
where

import API.Search.Util qualified as Search
import API.Team.Util
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.AWS qualified as AWS
import Brig.AWS.Types
import Brig.Options qualified as Opt
import Brig.Types.Activation
import Brig.Types.Intra
import Control.Arrow ((&&&))
import Control.Exception (throw)
import Control.Lens (ix, preview, (^.), (^?))
import Control.Monad.Catch
import Data.Aeson hiding (json)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.Aeson.Lens qualified as AesonL
import Data.ByteString qualified as C8
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.Domain
import Data.Either.Combinators
import Data.Handle
import Data.Id
import Data.Json.Util (fromUTCTimeMillis)
import Data.LegalHold
import Data.List.NonEmpty qualified as NonEmpty
import Data.List1 (singleton)
import Data.Misc (plainTextPassword6Unsafe)
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.String.Conversions
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Federator.MockServer (FederatedRequest (..), MockException (..))
import Imports hiding (head)
import Imports qualified
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Utilities.Error qualified as Error
import Network.Wai.Utilities.Error qualified as Wai
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import UnliftIO (mapConcurrently_)
import Util
import Util.AWS as Util
import Web.Cookie (parseSetCookie)
import Wire.API.Asset hiding (Asset)
import Wire.API.Asset qualified as Asset
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Routes.MultiTablePaging
import Wire.API.Team.Feature (ExposeInvitationURLsToTeamAdminConfig (..), FeatureStatus (..), FeatureTTL' (..), LockStatus (LockStatusLocked), withStatus)
import Wire.API.Team.Invitation (Invitation (inInvitation))
import Wire.API.Team.Permission hiding (self)
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Auth qualified as Auth
import Wire.API.User.Client

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> AWS.Env -> UserJournalWatcher -> TestTree
tests _ at opts p b c ch g aws userJournalWatcher =
  testGroup
    "account"
    [ test p "post /register - 201 (with preverified)" $ testCreateUserWithPreverified opts b userJournalWatcher,
      test p "testCreateUserWithInvalidVerificationCode - post /register - 400 (with preverified)" $ testCreateUserWithInvalidVerificationCode b,
      test p "post /register - 201" $ testCreateUser b g,
      test p "post /register - 400 + no email" $ testCreateUserNoEmailNoPassword b,
      test p "post /register - 201 anonymous" $ testCreateUserAnon b g,
      test p "testCreateUserEmptyName - post /register - 400 empty name" $ testCreateUserEmptyName b,
      test p "testCreateUserLongName - post /register - 400 name too long" $ testCreateUserLongName b,
      test p "post /register - 201 anonymous expiry" $ testCreateUserAnonExpiry b,
      test p "post /register - 201 pending" $ testCreateUserPending opts b,
      test p "testCreateUserConflict - post /register - 409 conflict" $ testCreateUserConflict opts b,
      test p "testCreateUserInvalidEmail - post /register - 400 invalid input" $ testCreateUserInvalidEmail opts b,
      test p "post /register - 403 blacklist" $ testCreateUserBlacklist opts b aws,
      test p "post /register - 400 external-SSO" $ testCreateUserExternalSSO b,
      test p "post /register - 403 restricted user creation" $ testRestrictedUserCreation opts b,
      test p "post /register - 403 too many members for legalhold" $ testTooManyMembersForLegalhold opts b,
      test p "post /activate - 200/204 + expiry" $ testActivateWithExpiry opts b at,
      test p "get /users/:uid - 404" $ testNonExistingUserUnqualified b,
      test p "get /users/<localdomain>/:uid - 404" $ testNonExistingUser b,
      test p "get /users/:domain/:uid - 422" $ testUserInvalidDomain b,
      test p "get /users/:uid - 200" $ testExistingUserUnqualified b,
      test p "get /users/<localdomain>/:uid - 200" $ testExistingUser b,
      test p "get /users?:id=.... - 200" $ testMultipleUsersUnqualified b,
      test p "head /users/:uid - 200" $ testUserExistsUnqualified b,
      test p "head /users/:uid - 404" $ testUserDoesNotExistUnqualified b,
      test p "head /users/:domain/:uid - 200" $ testUserExists b,
      test p "head /users/:domain/:uid - 404" $ testUserDoesNotExist b,
      test p "post /list-users@v3 - 200" $ testMultipleUsersV3 b,
      test p "post /list-users - 200" $ testMultipleUsers opts b,
      test p "put /self - 200" $ testUserUpdate b c userJournalWatcher,
      test p "put /access/self/email - 2xx" $ testEmailUpdate b userJournalWatcher,
      test p "head /self/password - 200/404" $ testPasswordSet b,
      test p "put /self/password - 400" $ testPasswordSetInvalidPasswordLength b,
      test p "put /self/password - 200" $ testPasswordChange b,
      test p "put /self/locale - 200" $ testUserLocaleUpdate b userJournalWatcher,
      test p "post /activate/send - 200" $ testSendActivationCode opts b,
      test p "post /activate/send - 400 invalid input" $ testSendActivationCodeInvalidEmailOrPhone b,
      test p "put /i/users/:uid/status (suspend)" $ testSuspendUser b,
      test p "get /i/users?:email - 200" $ testGetByIdentity b,
      -- "get /i/users?:ids=...&includePendingInvitations=..." is tested in 'testCreateUserNoIdP', 'testCreateUserTimeout'
      -- in spar's integration tests, module "Test.Spar.Scim.UserSpec"
      test p "delete/by-password" $ testDeleteUserByPassword b c userJournalWatcher,
      test p "delete/with-legalhold" $ testDeleteUserWithLegalHold b c userJournalWatcher,
      test p "delete/by-code" $ testDeleteUserByCode b,
      test p "delete/anonymous" $ testDeleteAnonUser b,
      test p "delete with profile pic" $ testDeleteWithProfilePic b ch,
      test p "put /i/users/:uid/sso-id" $ testUpdateSSOId b g,
      testGroup
        "temporary customer extensions"
        [ test p "domains blocked for registration" $ testDomainsBlockedForRegistration opts b
        ],
      testGroup
        "update user email by team owner"
        [ test p "put /users/:uid/email" $ testUpdateUserEmailByTeamOwner opts b
        ],
      testGroup
        "delete /i/users/:uid"
        [ test p "does nothing for completely deleted user" $ testDeleteUserWithCompletelyDeletedUser b c userJournalWatcher,
          test p "does nothing when the user doesn't exist" $ testDeleteUserWithNoUser b,
          test p "deletes a not deleted user" $ testDeleteUserWithNotDeletedUser b c userJournalWatcher,
          test p "delete again because of dangling property" $ testDeleteUserWithDanglingProperty b c userJournalWatcher
        ]
    ]

-- The testCreateUserWithInvalidVerificationCode test conforms to the following testing standards:
--
-- Registering with an invalid verification code and valid account details should fail.
testCreateUserWithInvalidVerificationCode :: Brig -> Http ()
testCreateUserWithInvalidVerificationCode brig = do
  -- Attempt to register (pre verified) user with phone
  p <- randomPhone
  code <- randomActivationCode -- incorrect but syntactically valid activation code
  let Object regPhone =
        object
          [ "name" .= Name "Alice",
            "phone" .= fromPhone p,
            "phone_code" .= code
          ]
  postUserRegister' regPhone brig !!! do
    const 400 === statusCode
    const (Just "invalid-phone") === fmap Wai.label . responseJsonMaybe

  -- Attempt to register (pre verified) user with email
  e <- randomEmail
  let Object regEmail =
        object
          [ "name" .= Name "Alice",
            "email" .= fromEmail e,
            "email_code" .= code
          ]
  postUserRegister' regEmail brig !!! const 404 === statusCode

testUpdateUserEmailByTeamOwner :: Opt.Opts -> Brig -> Http ()
testUpdateUserEmailByTeamOwner opts brig = do
  (_, teamOwner, emailOwner : otherTeamMember : _) <- createPopulatedBindingTeamWithNamesAndHandles brig 2
  (teamOwnerDifferentTeam, _) <- createUserWithTeam' brig
  newEmail <- randomEmail
  initiateEmailUpdateNoSend brig newEmail (userId emailOwner) !!! (const 202 === statusCode)
  checkActivationCode newEmail True
  checkLetActivationExpire newEmail
  checkActivationCode newEmail False
  checkSetUserEmail teamOwner emailOwner newEmail 200
  checkActivationCode newEmail True
  checkUnauthorizedRequests emailOwner otherTeamMember teamOwnerDifferentTeam newEmail
  activateEmail brig newEmail
  -- apparently activating the email does not invalidate the activation code
  -- therefore we let the activation code expire again
  checkLetActivationExpire newEmail
  checkSetUserEmail teamOwner emailOwner newEmail 200
  checkActivationCode newEmail False
  checkUnauthorizedRequests emailOwner otherTeamMember teamOwnerDifferentTeam newEmail
  checkActivationCode newEmail False
  where
    checkLetActivationExpire :: Email -> Http ()
    checkLetActivationExpire email = do
      let timeout = round (Opt.setActivationTimeout (Opt.optSettings opts))
      threadDelay ((timeout + 1) * 1000_000)
      checkActivationCode email False

    checkActivationCode :: Email -> Bool -> Http ()
    checkActivationCode email shouldExist = do
      maybeActivationCode <- Util.getActivationCode brig (Left email)
      void $
        lift $
          if shouldExist
            then assertBool "activation code should exists" (isJust maybeActivationCode)
            else assertBool "activation code should not exists" (isNothing maybeActivationCode)

    checkSetUserEmail :: User -> User -> Email -> Int -> Http ()
    checkSetUserEmail teamOwner emailOwner email expectedStatusCode =
      setUserEmail brig (userId teamOwner) (userId emailOwner) email !!! (const expectedStatusCode === statusCode)

    checkUnauthorizedRequests :: User -> User -> User -> Email -> Http ()
    checkUnauthorizedRequests emailOwner otherTeamMember teamOwnerDifferentTeam email = do
      setUserEmail brig (userId teamOwnerDifferentTeam) (userId emailOwner) email !!! (const 404 === statusCode)
      setUserEmail brig (userId otherTeamMember) (userId emailOwner) email !!! (const 403 === statusCode)

testCreateUserWithPreverified :: Opt.Opts -> Brig -> UserJournalWatcher -> Http ()
testCreateUserWithPreverified opts brig userJournalWatcher = do
  -- Register (pre verified) user with email
  e <- randomEmail
  let emailReq = RequestBodyLBS . encode $ object ["email" .= fromEmail e]
  post (brig . path "/activate/send" . contentJson . body emailReq)
    !!! (const 200 === statusCode)
  getActivationCode brig (Left e) >>= \case
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just (_, c) -> do
      let Object reg =
            object
              [ "name" .= Name "Alice",
                "email" .= fromEmail e,
                "email_code" .= c
              ]
      if Opt.setRestrictUserCreation (Opt.optSettings opts) == Just True
        then do
          postUserRegister' reg brig !!! const 403 === statusCode
        else do
          usr <- postUserRegister reg brig
          let uid = userId usr
          get (brig . path "/self" . zUser uid) !!! do
            const 200 === statusCode
            const (Just e) === (userEmail <=< responseJsonMaybe)
          Util.assertUserActivateJournaled userJournalWatcher usr "user activate"

testCreateUser :: Brig -> Galley -> Http () -- TODO: this has nothing to do with /register.  what's going on here?
testCreateUser brig galley = do
  uid <- userId <$> randomUser brig
  assertOnlySelfConversations galley uid

assertOnlySelfConversations :: Galley -> UserId -> Http ()
assertOnlySelfConversations galley uid = do
  page :: ConvIdsPage <-
    responseJsonError
      =<< post
        ( galley
            . paths ["conversations", "list-ids"]
            . json
              ( GetPaginatedConversationIds Nothing (toRange (Proxy @100)) ::
                  GetPaginatedConversationIds
              )
            . zAuthAccess uid "conn"
        )
        <!! const 200 === statusCode

  let results = mtpResults page
  -- check number of conversations
  liftIO $ length results @?= 2

  -- check conversation type
  r <-
    responseJsonError
      =<< post
        ( galley
            . zAuthAccess uid "conn"
            . paths ["conversations", "list"]
            . json (ListConversations (unsafeRange results))
        )
        <!! const 200 === statusCode
  for_ (crFound r) $ \conv ->
    liftIO $ cnvType conv @?= SelfConv

-- The testCreateUserEmptyName test conforms to the following testing standards:
--
-- An empty name is not allowed on registration
testCreateUserEmptyName :: Brig -> Http ()
testCreateUserEmptyName brig = do
  let p =
        RequestBodyLBS . encode $
          object
            ["name" .= ("" :: Text)]
  post (brig . path "/register" . contentJson . body p)
    !!! const 400 === statusCode

-- The testCreateUserLongName test conforms to the following testing standards:
--
-- a name with > 128 characters is not allowed.
testCreateUserLongName :: Brig -> Http ()
testCreateUserLongName brig = do
  let nameTooLong = cs $ concat $ replicate 129 "a"
  let p =
        RequestBodyLBS . encode $
          object
            ["name" .= (nameTooLong :: Text)]
  post (brig . path "/register" . contentJson . body p)
    !!! const 400 === statusCode

testCreateUserAnon :: Brig -> Galley -> Http ()
testCreateUserAnon brig galley = do
  let p =
        RequestBodyLBS . encode $
          object
            ["name" .= ("Mr. Pink" :: Text)]
  rs <-
    post (brig . path "/register" . contentJson . body p)
      <!! const 201 === statusCode
  -- Every registered user gets a cookie.
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
  liftIO $ assertBool "Missing zuid cookie" (isJust zuid)
  -- Every registered user gets two self conversations.
  let Just uid = userId <$> responseJsonMaybe rs
      Just quid = userQualifiedId <$> responseJsonMaybe rs
  assertOnlySelfConversations galley uid
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid quid "Mr. Pink"

testCreateUserPending :: Opt.Opts -> Brig -> Http ()
testCreateUserPending (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserPending _ brig = do
  e <- randomEmail
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Mr. Pink" :: Text),
              "email" .= fromEmail e,
              "password" .= defPassword
            ]
  rs <-
    post (brig . path "/register" . contentJson . body p)
      <!! const 201 === statusCode
  -- Even though activation is pending, the user gets an access cookie,
  -- i.e. every user starts out as a "Wireless" user.
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
  liftIO $ assertBool "Missing zuid cookie" (isJust zuid)
  -- Cannot login via email (pending activation)
  login brig (defEmailLogin e) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "pending-activation") === fmap Error.label . responseJsonMaybe
  -- The user has no verified / activated identity yet
  let Just uid = userId <$> responseJsonMaybe rs
      Just quid = userQualifiedId <$> responseJsonMaybe rs
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just True) === \rs' -> do
      self <- responseJsonMaybe rs'
      pure $! isNothing (userIdentity (selfUser self))
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid quid "Mr. Pink"

testCreateUserNoEmailNoPassword :: Brig -> Http ()
testCreateUserNoEmailNoPassword brig = do
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p
            ]
  post
    (brig . path "/i/users" . contentJson . body newUser)
    !!! do
      const 400 === statusCode
      (const (Just "invalid-phone") === fmap Error.label . responseJsonMaybe)

-- The testCreateUserConflict test conforms to the following testing standards:
--
-- email address must not be taken on @/register@.
testCreateUserConflict :: Opt.Opts -> Brig -> Http ()
testCreateUserConflict (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserConflict _ brig = do
  -- trusted email domains
  u <- createUser "conflict" brig
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("conflict1" :: Text),
              "email" .= (fromEmail <$> userEmail u), -- dup. email
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p) !!! do
    const 409 === statusCode
    const (Just "key-exists") === fmap Error.label . responseJsonMaybe
  -- untrusted email domains
  u2 <- createUserUntrustedEmail "conflict" brig
  let Just (Email loc dom) = userEmail u2
  let p2 =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("conflict2" :: Text),
              "email" .= (T.takeWhile (/= '+') loc <> "@" <> dom), -- dup. email
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p2) !!! do
    const 409 === statusCode
    const (Just "key-exists") === fmap Error.label . responseJsonMaybe

-- The testCreateUserInvalidEmail test conforms to the following testing standards:
--
-- Test to make sure a new user cannot be created with an invalid email address or invalid phone number.
testCreateUserInvalidEmail :: Opt.Opts -> Brig -> Http ()
testCreateUserInvalidEmail (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserInvalidEmail _ brig = do
  email <- randomEmail
  let reqEmail =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("foo" :: Text),
              "email" .= fromEmail email,
              "password" .= defPassword,
              "phone" .= ("123456" :: Text) -- invalid phone number, but ignored
            ]
  post (brig . path "/register" . contentJson . body reqEmail)
    !!! const 201 === statusCode

  phone <- randomPhone
  let reqPhone =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("foo" :: Text),
              "email" .= ("invalid@email" :: Text), -- invalid since there's only a single label
              "password" .= defPassword,
              "phone" .= fromPhone phone
            ]
  post (brig . path "/register" . contentJson . body reqPhone)
    !!! const 400 === statusCode

testCreateUserBlacklist :: Opt.Opts -> Brig -> AWS.Env -> Http ()
testCreateUserBlacklist (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ _ = pure ()
testCreateUserBlacklist _ brig aws =
  mapM_ ensureBlacklist ["bounce", "complaint"]
  where
    ensureBlacklist typ = do
      e <- randomEmail
      flip finally (removeBlacklist brig e) $ do
        post (brig . path "/register" . contentJson . body (p e)) !!! const 201 === statusCode
        -- If we are using a local env, we need to fake this bounce
        unless (Util.isRealSESEnv aws) $
          forceBlacklist typ e
        -- Typically bounce/complaint messages arrive instantaneously
        awaitBlacklist 30 e
        post (brig . path "/register" . contentJson . body (p e)) !!! do
          const 403 === statusCode
          const (Just "blacklisted-email") === fmap Error.label . responseJsonMaybe
    p email =
      RequestBodyLBS . encode $
        object
          [ "name" .= ("Alice" :: Text),
            "email" .= email,
            "password" .= defPassword
          ]
    -- If there is no queue available, we need to force it either by publishing an event or using the API
    forceBlacklist :: Text -> Email -> Http ()
    forceBlacklist typ em = case aws ^. AWS.sesQueue of
      Just queue -> publishMessage typ em queue
      Nothing -> Bilge.post (brig . path "i/users/blacklist" . queryItem "email" (toByteString' em)) !!! const 200 === statusCode
    publishMessage :: Text -> Email -> Text -> Http ()
    publishMessage typ em queue = do
      let bdy = encode $ case typ of
            "bounce" -> MailBounce BouncePermanent [em]
            "complaint" -> MailComplaint [em]
            x -> error ("Unsupported message type: " ++ show x)
      void . AWS.execute aws $ AWS.enqueueStandard queue bdy
    awaitBlacklist :: Int -> Email -> Http ()
    awaitBlacklist n e = do
      r <- Bilge.head (brig . path "i/users/blacklist" . queryItem "email" (toByteString' e))
      when (statusCode r == 404 && n > 0) $ do
        liftIO $ threadDelay 1000000
        awaitBlacklist (n - 1) e

testCreateUserExternalSSO :: Brig -> Http ()
testCreateUserExternalSSO brig = do
  teamid <- Id <$> liftIO UUID.nextRandom
  let ssoid = UserSSOId mkSimpleSampleUref
      p withsso withteam =
        RequestBodyLBS . encode . object $
          ["name" .= ("foo" :: Text)]
            <> ["sso_id" .= Just ssoid | withsso]
            <> ["team_id" .= Just teamid | withteam]
  post (brig . path "/register" . contentJson . body (p False True))
    !!! const 400 === statusCode
  post (brig . path "/register" . contentJson . body (p True False))
    !!! const 400 === statusCode
  post (brig . path "/register" . contentJson . body (p True True))
    !!! const 400 === statusCode

testActivateWithExpiry :: Opt.Opts -> Brig -> Opt.Timeout -> Http ()
testActivateWithExpiry (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ _ = pure ()
testActivateWithExpiry _ brig timeout = do
  u <- responseJsonError =<< registerUser "dilbert" brig
  let email = fromMaybe (error "missing email") (userEmail u)
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> do
      activate brig kc !!! do
        const 200 === statusCode
        const (Just (userIdentity u, True)) === actualBody
      -- Note: This value must be larger than the option passed as `activation-timeout`
      awaitExpiry (round timeout + 5) kc
      activate brig kc !!! const 404 === statusCode
  where
    actualBody :: (HasCallStack) => ResponseLBS -> Maybe (Maybe UserIdentity, Bool)
    actualBody rs = do
      a <- responseJsonMaybe rs
      Just (Just (activatedIdentity a), activatedFirst a)
    awaitExpiry :: (HasCallStack) => Int -> ActivationPair -> Http ()
    awaitExpiry n kc = do
      liftIO $ threadDelay 1000000
      r <- activate brig kc
      when (statusCode r == 204 && n > 0) $
        awaitExpiry (n - 1) kc

testNonExistingUserUnqualified :: Brig -> Http ()
testNonExistingUserUnqualified brig = do
  findingOne <- liftIO $ Id <$> UUID.nextRandom
  foundOne <- liftIO $ Id <$> UUID.nextRandom
  get (apiVersion "v1" . brig . paths ["users", pack $ show foundOne] . zUser findingOne)
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe
  get (apiVersion "v1" . brig . paths ["users", pack $ show foundOne] . zUser foundOne)
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testNonExistingUser :: Brig -> Http ()
testNonExistingUser brig = do
  qself <- userQualifiedId <$> randomUser brig
  uid1 <- liftIO $ Id <$> UUID.nextRandom
  uid2 <- liftIO $ Id <$> UUID.nextRandom
  let uid = qUnqualified qself
      domain = qDomain qself
  get (apiVersion "v1" . brig . paths ["users", toByteString' domain, toByteString' uid1] . zUser uid)
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe
  get (apiVersion "v1" . brig . paths ["users", toByteString' domain, toByteString' uid2] . zUser uid)
    !!! do
      const 404 === statusCode
      const (Just "not-found") === fmap Error.label . responseJsonMaybe

testUserInvalidDomain :: Brig -> Http ()
testUserInvalidDomain brig = do
  qself <- userQualifiedId <$> randomUser brig
  let uid = qUnqualified qself
  get (brig . paths ["users", "invalid.example.com", toByteString' uid] . zUser uid)
    !!! do
      const 422 === statusCode
      const (Just "/federation/api-version")
        === preview (ix "data" . ix "path") . responseJsonUnsafe @Value
      const (Just "invalid.example.com")
        === preview (ix "data" . ix "domain") . responseJsonUnsafe @Value

testExistingUserUnqualified :: Brig -> Http ()
testExistingUserUnqualified brig = do
  uid <- userId <$> randomUser brig
  get (apiVersion "v1" . brig . paths ["users", pack $ show uid] . zUser uid) !!! do
    const 200 === statusCode
    const (Just uid)
      === ( \r -> do
              b <- responseBody r
              b ^? key "id" >>= maybeFromJSON
          )

testExistingUser :: Brig -> Http ()
testExistingUser brig = do
  quser <- userQualifiedId <$> randomUser brig
  let uid = qUnqualified quser
      domain = qDomain quser
  get
    ( apiVersion "v1"
        . brig
        . zUser uid
        . paths
          [ "users",
            toByteString' domain,
            toByteString' uid
          ]
    )
    !!! do
      const 200 === statusCode
      const (Just uid)
        === ( \r -> do
                b <- responseBody r
                b ^? key "id" >>= maybeFromJSON
            )

testUserExistsUnqualified :: Brig -> Http ()
testUserExistsUnqualified brig = do
  qself <- userQualifiedId <$> randomUser brig
  quser <- userQualifiedId <$> randomUser brig
  head
    ( apiVersion "v1"
        . brig
        . paths ["users", toByteString' (qUnqualified quser)]
        . zUser (qUnqualified qself)
    )
    !!! do
      const 200 === statusCode
      const mempty === responseBody

testUserDoesNotExistUnqualified :: Brig -> Http ()
testUserDoesNotExistUnqualified brig = do
  qself <- userQualifiedId <$> randomUser brig
  uid <- liftIO $ Id <$> UUID.nextRandom
  head
    ( brig
        . paths ["users", toByteString' uid]
        . zUser (qUnqualified qself)
    )
    !!! do
      const 404 === statusCode
      const mempty === responseBody

testUserExists :: Brig -> Http ()
testUserExists brig = do
  qself <- userQualifiedId <$> randomUser brig
  quser <- userQualifiedId <$> randomUser brig
  head
    ( brig
        . paths ["users", toByteString' (qDomain quser), toByteString' (qUnqualified quser)]
        . zUser (qUnqualified qself)
    )
    !!! do
      const 200 === statusCode
      const mempty === responseBody

testUserDoesNotExist :: Brig -> Http ()
testUserDoesNotExist brig = do
  qself <- userQualifiedId <$> randomUser brig
  uid <- liftIO $ Id <$> UUID.nextRandom
  head
    ( brig
        . paths ["users", toByteString' (qDomain qself), toByteString' uid]
        . zUser (qUnqualified qself)
    )
    !!! do
      const 404 === statusCode
      const mempty === responseBody

testMultipleUsersUnqualified :: Brig -> Http ()
testMultipleUsersUnqualified brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  u3 <- createAnonUser "a" brig
  let uids = C8.intercalate "," $ map (toByteString' . userId) [u1, u2, u3]
      -- n.b. email addresses and phone numbers are never returned
      -- on this endpoint, only from the self profile (/self).
      expected =
        Set.fromList
          [ (Just $ userDisplayName u1, Nothing :: Maybe Email),
            (Just $ userDisplayName u2, Nothing),
            (Just $ userDisplayName u3, Nothing)
          ]
  get
    ( apiVersion "v1"
        . brig
        . zUser (userId u1)
        . contentJson
        . path "users"
        . queryItem "ids" uids
    )
    !!! do
      const 200 === statusCode
      const (Just expected) === result
  where
    result r =
      Set.fromList
        . map (field "name" &&& field "email")
        <$> responseJsonMaybe r
    field :: (FromJSON a) => Key -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON

testMultipleUsersV3 :: Brig -> Http ()
testMultipleUsersV3 brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  u3 <- createAnonUser "a" brig
  let users = [u1, u2, u3]
      q = ListUsersByIds (map userQualifiedId users)
      expected =
        Set.fromList
          [ (Just $ userDisplayName u1, Nothing :: Maybe Email),
            (Just $ userDisplayName u2, Nothing),
            (Just $ userDisplayName u3, Nothing)
          ]
  post
    ( apiVersion "v3"
        . brig
        . zUser (userId u1)
        . contentJson
        . path "list-users"
        . body (RequestBodyLBS (Aeson.encode q))
    )
    !!! do
      const 200 === statusCode
      const (Just expected) === result
  where
    result r =
      Set.fromList
        . map (field "name" &&& field "email")
        <$> responseJsonMaybe r
    field :: (FromJSON a) => Key -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON

testMultipleUsers :: Opt.Opts -> Brig -> Http ()
testMultipleUsers opts brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  u3 <- createAnonUser "a" brig
  -- A remote user that can't be listed
  u4 <- Qualified <$> randomId <*> pure (Domain "far-away.example.com")
  -- A remote user that can be listed
  let evenFurtherAway = Domain "even-further-away.example.com"
  u5 <- Qualified <$> randomId <*> pure evenFurtherAway
  let u5Profile =
        UserProfile
          { profileQualifiedId = u5,
            profileName = Name "u5",
            profileTextStatus = Nothing,
            profilePict = Pict [],
            profileAssets = [],
            profileAccentId = ColourId 0,
            profileDeleted = False,
            profileService = Nothing,
            profileHandle = Nothing,
            profileExpire = Nothing,
            profileTeam = Nothing,
            profileEmail = Nothing,
            profileLegalholdStatus = UserLegalHoldDisabled,
            profileSupportedProtocols = defSupportedProtocols
          }
      users = [u1, u2, u3]
      q = ListUsersByIds $ u5 : u4 : map userQualifiedId users
      expected =
        Set.fromList
          [ (Just $ userDisplayName u1, Nothing :: Maybe Email),
            (Just $ userDisplayName u2, Nothing),
            (Just $ userDisplayName u3, Nothing),
            (Just $ profileName u5Profile, profileEmail u5Profile)
          ]
      expectedFailed = Set.fromList [u4]

  let fedMockResponse req = do
        -- Check that our allowed remote user is being asked for
        if frTargetDomain req == evenFurtherAway
          then -- Return the data for u5
            pure $ encode [u5Profile]
          else -- Otherwise mock an unavailable federation server
            throw $ MockErrorResponse Http.status500 "Down for maintenance"
      -- Galley isn't needed, but this is what mock federators are available.
      galleyHandler _ = error "not mocked"
  (response, _rpcCalls, _galleyCalls) <- liftIO $
    withMockedFederatorAndGalley opts (Domain "example.com") fedMockResponse galleyHandler $ do
      post
        ( brig
            . zUser (userId u1)
            . contentJson
            . path "list-users"
            . body (RequestBodyLBS (Aeson.encode q))
        )

  pure response !!! do
    const 200 === statusCode
    const (Just expected) === result
    const (pure $ pure expectedFailed) === resultFailed
  where
    result r =
      Set.fromList
        . map (\u -> (pure $ profileName u, profileEmail u))
        . listUsersByIdFound
        <$> responseJsonMaybe r
    resultFailed r = fmap (Set.fromList . NonEmpty.toList) . listUsersByIdFailed <$> responseJsonMaybe r

testCreateUserAnonExpiry :: Brig -> Http ()
testCreateUserAnonExpiry b = do
  u1 <- randomUser b
  alice <- randomUser b
  bob <- createAnonUserExpiry (Just 5 {- 2 was flaky, so it's 5 now; make sure to re-align with 'awaitExpiry' below! -}) "bob" b
  liftIO $ assertBool "expiry not set on regular creation" (isNothing (userExpire alice))
  ensureExpiry (fromUTCTimeMillis <$> userExpire bob) "bob/register"
  resAlice <- getProfile (userId u1) (userId alice)
  resBob <- getProfile (userId u1) (userId bob)
  selfBob <- get (b . zUser (userId bob) . path "self") <!! const 200 === statusCode
  liftIO $ assertBool "Bob must not be in a deleted state initially" (maybe True not (deleted selfBob))
  liftIO $ assertBool "Regular user should not have any expiry" (null $ expire resAlice)
  ensureExpiry (expire resBob) "bob/public"
  ensureExpiry (expire selfBob) "bob/self"
  awaitExpiry 10 (userId u1) (userId bob)
  resBob' <- getProfile (userId u1) (userId bob)
  liftIO $ assertBool "Bob must be in deleted state" (fromMaybe False $ deleted resBob')
  where
    getProfile :: UserId -> UserId -> Http ResponseLBS
    getProfile zusr uid = get (apiVersion "v1" . b . zUser zusr . paths ["users", toByteString' uid]) <!! const 200 === statusCode

    awaitExpiry :: Int -> UserId -> UserId -> Http ()
    awaitExpiry n zusr uid = do
      -- after expiration, a profile lookup should trigger garbage collection of ephemeral users
      r <- getProfile zusr uid
      when (statusCode r == 200 && isNothing (deleted r) && n > 0) $ do
        liftIO $ threadDelay 1000000
        awaitExpiry (n - 1) zusr uid

    ensureExpiry :: Maybe UTCTime -> String -> Http ()
    ensureExpiry expiry s = do
      now <- liftIO getCurrentTime
      case expiry of
        Nothing -> liftIO $ assertFailure ("user must have an expiry" <> s)
        Just a -> do
          let diff = diffUTCTime a now
              minExp = 1 :: Integer -- 1 second
              maxExp = 60 * 60 * 24 * 10 :: Integer -- 10 days
          liftIO $ assertBool "expiry must be in the future" (diff >= fromIntegral minExp)
          liftIO $ assertBool "expiry must be less than 10 days" (diff < fromIntegral maxExp)

    expire :: ResponseLBS -> Maybe UTCTime
    expire r = field "expires_at" =<< responseJsonMaybe r

    deleted :: ResponseLBS -> Maybe Bool
    deleted r = field "deleted" =<< responseJsonMaybe r

    field :: (FromJSON a) => Key -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON

testUserUpdate :: (HasCallStack) => Brig -> Cannon -> UserJournalWatcher -> Http ()
testUserUpdate brig cannon userJournalWatcher = do
  aliceUser <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher aliceUser "user create alice"
  let alice = userId aliceUser
      aliceQ = userQualifiedId aliceUser
  bobUser <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher bobUser "user create bob"
  let bob = userId bobUser
  aliceNewName <- randomName
  connectUsers brig alice (singleton bob)
  let newColId = Just 5
      newAssets =
        Just
          [ ImageAsset
              (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring)
              (Just AssetComplete)
          ]
      mNewName = Just $ aliceNewName
      mNewTextStatus = rightToMaybe $ mkTextStatus "fun status"
      newPic = Nothing -- Legacy
      userUpdate = UserUpdate mNewName mNewTextStatus newPic newAssets newColId
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile & receive notification
  WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
    put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update)
      !!! const 200 === statusCode
    liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]
    -- Should generate a user update journaled event with the new name
    Util.assertNameUpdateJournaled userJournalWatcher alice aliceNewName "alice name update"
  -- get the updated profile
  get (brig . path "/self" . zUser alice) !!! do
    const 200 === statusCode
    const (mNewName, mNewTextStatus, newColId, newAssets)
      === ( \u ->
              ( fmap userDisplayName u,
                userTextStatus =<< u,
                fmap userAccentId u,
                fmap userAssets u
              )
          )
        . responseJsonMaybe
  -- should appear in search by 'newName'
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCanFind brig suid aliceQ (fromName aliceNewName)

-- This tests the behavior of `/i/self/email` instead of `/self/email` or
-- `/access/self/email`.  tests for session token handling under `/access/self/email` are in
-- `services/brig/test/integration/API/User/Auth.hs`.
testEmailUpdate :: Brig -> UserJournalWatcher -> Http ()
testEmailUpdate brig userJournalWatcher = do
  usr <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher usr "user create random"
  let uid = userId usr
  eml <- randomEmail
  -- update email
  let Just oldeml = userEmail usr
  initiateEmailUpdateLogin brig eml (emailLogin oldeml defPassword Nothing) uid !!! const 202 === statusCode
  -- activate
  activateEmail brig eml
  checkEmail brig uid eml
  Util.assertEmailUpdateJournaled userJournalWatcher uid eml "user update"
  -- update email, which is exactly the same as before (idempotency)
  initiateEmailUpdateLogin brig eml (emailLogin eml defPassword Nothing) uid !!! const 204 === statusCode
  -- ensure no other user has "test+<uuid>@example.com"
  -- if there is such a user, let's delete it first.  otherwise
  -- this test fails since there can be only one user with "test+...@example.com"
  ensureNoOtherUserWithEmail (Email "test" "example.com")
  -- we want to use a non-trusted domain in order to verify profile changes
  flip initiateUpdateAndActivate uid =<< mkEmailRandomLocalSuffix "test@example.com"
  flip initiateUpdateAndActivate uid =<< mkEmailRandomLocalSuffix "test@example.com"

  -- adding a clean-up step seems to avoid the subsequent failures.
  -- If subsequent runs start failing, it's possible that the aggressive setting
  -- for `setSuspendInactiveUsers` is triggering before we can clean that user up.
  -- In that case, you might need to manually delete the user from the test DB. @elland
  deleteUserInternal uid brig !!! const 202 === statusCode
  where
    ensureNoOtherUserWithEmail :: Email -> Http ()
    ensureNoOtherUserWithEmail eml = do
      tk :: Maybe AccessToken <-
        responseJsonMaybe <$> login brig (defEmailLogin eml) SessionCookie
      for_ tk $ \t -> do
        deleteUser (Auth.user t) (Just defPassword) brig !!! const 200 === statusCode
        Util.assertDeleteJournaled userJournalWatcher (Auth.user t) "user deletion"

    initiateUpdateAndActivate :: Email -> UserId -> Http ()
    initiateUpdateAndActivate eml uid = do
      initiateEmailUpdateNoSend brig eml uid !!! const 202 === statusCode
      activateEmail brig eml
      checkEmail brig uid eml
      Util.assertEmailUpdateJournaled userJournalWatcher uid eml "user update"
      -- Ensure login work both with the full email and the "short" version
      login brig (defEmailLogin eml) SessionCookie !!! const 200 === statusCode
      login brig (defEmailLogin (Email "test" "example.com")) SessionCookie !!! const 200 === statusCode

testUserLocaleUpdate :: Brig -> UserJournalWatcher -> Http ()
testUserLocaleUpdate brig userJournalWatcher = do
  usr <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher usr "user create random"
  let uid = userId usr
  -- update locale info with locale supported in templates
  let locEN = fromMaybe (error "Failed to parse locale") $ parseLocale "en-US"
  put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale locEN)
    !!! const 200 === statusCode
  get (brig . path "/self" . contentJson . zUser uid . zConn "c")
    !!! do
      const 200 === statusCode
      const (Just locEN) === (Just . userLocale . selfUser <=< responseJsonMaybe)
  Util.assertLocaleUpdateJournaled userJournalWatcher uid locEN "user update"
  -- update locale info with locale NOT supported in templates
  let locPT = fromMaybe (error "Failed to parse locale") $ parseLocale "pt-PT"
  put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale locPT)
    !!! const 200 === statusCode
  Util.assertLocaleUpdateJournaled userJournalWatcher uid locPT "user update"
  -- get the updated locale
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (parseLocale "pt-PT") === (Just . userLocale . selfUser <=< responseJsonMaybe)
  where
    locale l = body . RequestBodyLBS . encode $ object ["locale" .= l]

testSuspendUser :: Brig -> Http ()
testSuspendUser brig = do
  u <- randomUser brig
  let uid = userId u
      quid = userQualifiedId u
      Just email = userEmail u
  setStatus brig uid Suspended
  -- login fails
  login brig (defEmailLogin email) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "suspended") === fmap Error.label . responseJsonMaybe
  -- check status
  chkStatus brig uid Suspended
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid quid (fromName (userDisplayName u))
  -- re-activate
  setStatus brig uid Active
  chkStatus brig uid Active
  -- should appear in search again
  Search.refreshIndex brig
  Search.assertCanFind brig suid quid (fromName (userDisplayName u))

testGetByIdentity :: Brig -> Http ()
testGetByIdentity brig = do
  e <- randomEmail
  let emailBs = T.encodeUtf8 $ fromEmail e
      newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "email" .= fromEmail e
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  get (brig . zUser uid . path "i/users" . queryItem "email" emailBs) !!! do
    const 200 === statusCode
    const (Just [uid]) === getUids
  where
    getUids r = fmap (userId . accountUser) <$> responseJsonMaybe r

testPasswordSet :: Brig -> Http ()
testPasswordSet brig = do
  e <- randomEmail
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "email" .= fromEmail e
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  -- No password set yet
  Bilge.head (brig . path "/self/password" . zUser uid)
    !!! const 404 === statusCode
  -- Since there is no password set, we can just set one
  put (brig . path "/self/password" . contentJson . zUser uid . body pwSet)
    !!! const 200 === statusCode
  -- Now we should have a password
  Bilge.head (brig . path "/self/password" . zUser uid)
    !!! const 200 === statusCode
  where
    pwSet =
      RequestBodyLBS . encode $
        object
          [ "new_password" .= ("a_very_long_password" :: Text)
          ]

testPasswordSetInvalidPasswordLength :: Brig -> Http ()
testPasswordSetInvalidPasswordLength brig = do
  e <- randomEmail
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "email" .= fromEmail e
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  put (brig . path "/self/password" . contentJson . zUser uid . body shortPassword)
    !!! const 400 === statusCode
  where
    shortPassword =
      RequestBodyLBS . encode $
        object
          [ "new_password" .= ("secret" :: Text)
          ]

testPasswordChange :: Brig -> Http ()
testPasswordChange brig = do
  (uid, Just email) <- (userId &&& userEmail) <$> randomUser brig
  put (brig . path "/self/password" . contentJson . zUser uid . body pwChange)
    !!! const 200 === statusCode
  -- login with new password
  login
    brig
    (PasswordLogin (PasswordLoginData (LoginByEmail email) newPass Nothing Nothing))
    PersistentCookie
    !!! const 200 === statusCode
  -- try to change the password to itself should fail
  put (brig . path "/self/password" . contentJson . zUser uid . body pwChange')
    !!! const 409 === statusCode
  -- Setting a password for an anonymous / unverified user should fail
  uid2 <- userId <$> createAnonUser "foo2" brig
  put (brig . path "/self/password" . contentJson . zUser uid2 . body pwSet)
    !!! (const 403 === statusCode)
  where
    newPass = plainTextPassword6Unsafe "topsecret"
    pwChange =
      RequestBodyLBS . encode $
        object
          [ "old_password" .= defPassword,
            "new_password" .= newPass
          ]
    pwChange' =
      RequestBodyLBS . encode $
        object
          [ "old_password" .= newPass,
            "new_password" .= newPass
          ]
    pwSet =
      RequestBodyLBS . encode $
        object
          [ "new_password" .= newPass
          ]

testSendActivationCode :: Opt.Opts -> Brig -> Http ()
testSendActivationCode opts brig = do
  -- Code for phone pre-verification
  requestActivationCode brig 400 . Right =<< randomPhone
  -- Code for email pre-verification
  requestActivationCode brig 200 . Left =<< randomEmail
  -- Standard email registration flow
  if Opt.setRestrictUserCreation (Opt.optSettings opts) == Just True
    then do
      registerUser "Alice" brig !!! const 403 === statusCode
    else do
      r <- registerUser "Alice" brig <!! const 201 === statusCode
      let Just email = userEmail =<< responseJsonMaybe r
      -- Re-request existing activation code
      requestActivationCode brig 200 (Left email)

testSendActivationCodeInvalidEmailOrPhone :: Brig -> Http ()
testSendActivationCodeInvalidEmailOrPhone brig = do
  let Just invalidEmail = parseEmail "?@?"
  let invalidPhone = Phone "1234"
  -- Code for phone pre-verification
  requestActivationCode brig 400 (Right invalidPhone)
  -- Code for email pre-verification
  requestActivationCode brig 400 (Left invalidEmail)

testDeleteUserByPassword :: Brig -> Cannon -> UserJournalWatcher -> Http ()
testDeleteUserByPassword brig cannon userJournalWatcher = do
  u <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher u "user activate"
  let uid1 = userId u
  let email = fromMaybe (error "Missing email") (userEmail u)
  -- Establish some connections
  usr2 <- randomUser brig
  let uid2 = userId usr2
  Util.assertUserActivateJournaled userJournalWatcher usr2 "user activate"
  usr3 <- randomUser brig
  let uid3 = userId usr3
  Util.assertUserActivateJournaled userJournalWatcher usr3 "user activate"
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  postConnection brig uid1 uid3 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  putConnection brig uid3 uid1 Accepted !!! const 200 === statusCode
  postConnection brig uid2 uid3 !!! const 201 === statusCode
  con32 <- putConnection brig uid3 uid2 Accepted <!! const 200 === statusCode
  con23 <- getConnection brig uid2 uid3 <!! const 200 === statusCode
  -- Register a client
  addClient brig uid1 (defNewClient PermanentClientType [Imports.head somePrekeys] (Imports.head someLastPrekeys))
    !!! const 201 === statusCode
  -- Initial login
  login brig (defEmailLogin email) PersistentCookie
    !!! const 200 === statusCode
  n1 <- countCookies brig uid1 defCookieLabel
  liftIO $ Just 1 @=? n1
  -- Initiate a change of email address, to verify that activation
  -- does not work after the account has been deleted.
  eml <- randomEmail
  let Just oldeml = userEmail u
  initiateEmailUpdateLogin brig eml (emailLogin oldeml defPassword Nothing) uid1 !!! (const 202 === statusCode)
  setHandleAndDeleteUser brig cannon u [uid2, uid3] userJournalWatcher $
    \uid -> deleteUser uid (Just defPassword) brig !!! const 200 === statusCode
  -- Activating the new email address now should not work
  act <- getActivationCode brig (Left eml)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 404 === statusCode
        const (Just "invalid-code") === fmap Error.label . responseJsonMaybe
  -- Connections involving uid1 are gone (uid2 <-> uid3 remains)
  let u1Conns = UserConnectionList [] False
  let u2Conns = UserConnectionList (maybeToList (responseJsonMaybe con23)) False
  let u3Conns = UserConnectionList (maybeToList (responseJsonMaybe con32)) False
  listConnections brig uid1 !!! do
    const 200 === statusCode
    const (Just u1Conns) === responseJsonMaybe
  listConnections brig uid2 !!! do
    const 200 === statusCode
    const (Just u2Conns) === responseJsonMaybe
  listConnections brig uid3 !!! do
    const 200 === statusCode
    const (Just u3Conns) === responseJsonMaybe

testDeleteUserWithLegalHold :: Brig -> Cannon -> UserJournalWatcher -> Http ()
testDeleteUserWithLegalHold brig cannon userJournalWatcher = do
  user <- randomUser brig
  let uid = userId user
  -- Register a legalhold client
  addClientInternal brig uid (defNewClient LegalHoldClientType [Imports.head somePrekeys] (Imports.head someLastPrekeys))
    !!! const 201 === statusCode
  Util.assertUserActivateJournaled userJournalWatcher user "user activate testDeleteInternal1: "
  setHandleAndDeleteUser brig cannon user [] userJournalWatcher $
    \uid' -> deleteUser uid' (Just defPassword) brig !!! const 200 === statusCode

testDeleteUserByCode :: Brig -> Http ()
testDeleteUserByCode brig = do
  u <- randomUser brig
  deleteUser (userId u) Nothing brig
    !!! const 202 === statusCode
  -- (Syntactically) invalid key / code
  let _key = "" :: Text
  let _code = "123" :: Text
  send _key _code !!! do
    const 400 === statusCode
    const (Just "bad-request") === fmap Error.label . responseJsonMaybe
  -- (Semantically) invalid key / code
  let _key = T.replicate 20 "x"
  let _code = "idontknow" :: Text
  send _key _code !!! do
    const 403 === statusCode
    const (Just "invalid-code") === fmap Error.label . responseJsonMaybe
  where
    send k c = post (brig . path "/delete" . contentJson . body (payload k c))
    payload k c =
      RequestBodyLBS . encode $
        object
          ["key" .= k, "code" .= c]

testDeleteAnonUser :: Brig -> Http ()
testDeleteAnonUser brig = do
  uid <- userId <$> createAnonUser "anon" brig
  deleteUser uid Nothing brig
    !!! const 200 === statusCode

testDeleteWithProfilePic :: Brig -> CargoHold -> Http ()
testDeleteWithProfilePic brig cargohold = do
  email <- randomEmail
  -- Users need to be verified if they want to upload assets, so email it is!
  uid <- userId <$> createUserWithEmail "anon" email brig
  ast <- responseJsonError =<< uploadAsset cargohold uid Asset.defAssetSettings "this is my profile pic"
  -- Ensure that the asset is there
  downloadAsset cargohold uid (ast ^. Asset.assetKey) !!! const 200 === statusCode
  let newAssets =
        Just
          [ ImageAsset
              (qUnqualified $ ast ^. Asset.assetKey)
              (Just AssetComplete)
          ]
      userUpdate = UserUpdate Nothing Nothing Nothing newAssets Nothing
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile with the uploaded asset
  put (brig . path "/self" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode
  deleteUser uid (pure defPassword) brig !!! const 200 === statusCode
  -- Check that the asset gets deleted
  downloadAsset cargohold uid (ast ^. Asset.assetKey) !!! const 404 === statusCode

testUpdateSSOId :: Brig -> Galley -> Http ()
testUpdateSSOId brig galley = do
  noSuchUserId <- Id <$> liftIO UUID.nextRandom
  put
    ( brig
        . paths ["i", "users", toByteString' noSuchUserId, "sso-id"]
        . Bilge.json (UserSSOId (mkSampleUref "1" "1"))
    )
    !!! const 404 === statusCode
  let go :: (HasCallStack) => User -> UserSSOId -> Http ()
      go user ssoid = do
        let uid = userId user
        put
          ( brig
              . paths ["i", "users", toByteString' uid, "sso-id"]
              . Bilge.json ssoid
          )
          !!! const 200 === statusCode
        profile :: SelfProfile <- responseJsonError =<< get (brig . path "/self" . zUser uid)
        let Just (SSOIdentity ssoid' mEmail) = userIdentity . selfUser $ profile
        liftIO $ do
          assertEqual "updateSSOId/ssoid" ssoid ssoid'
          assertEqual "updateSSOId/email" (userEmail user) mEmail
  (owner, teamid) <- createUserWithTeam brig
  let mkMember :: Bool -> Http User
      mkMember hasEmail = do
        member <- createTeamMember brig galley owner teamid noPermissions
        unless hasEmail $ do
          error "not implemented"
        selfUser <$> (responseJsonError =<< get (brig . path "/self" . zUser (userId member)))
  let ssoids1 = [UserSSOId (mkSampleUref "1" "1"), UserSSOId (mkSampleUref "1" "2")]
      ssoids2 = [UserSSOId (mkSampleUref "2" "1"), UserSSOId (mkSampleUref "2" "2")]
  users <-
    sequence
      [ mkMember True
      -- the following two could be implemented by creating the user implicitly via SSO login.
      -- , mkMember False  False
      ]
  zipWithM_ go users ssoids1
  zipWithM_ go users ssoids2

testDomainsBlockedForRegistration :: Opt.Opts -> Brig -> Http ()
testDomainsBlockedForRegistration opts brig = withDomainsBlockedForRegistration opts ["bad1.domain.com", "bad2.domain.com"] $ do
  badEmail1 <- randomEmail <&> \e -> e {emailDomain = "bad1.domain.com"}
  badEmail2 <- randomEmail <&> \e -> e {emailDomain = "bad2.domain.com"}
  post (brig . path "/activate/send" . contentJson . body (p badEmail1)) !!! do
    const 451 === statusCode
    const (Just "domain-blocked-for-registration") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)
  post (brig . path "/activate/send" . contentJson . body (p badEmail2)) !!! do
    const 451 === statusCode
    const (Just "domain-blocked-for-registration") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)
  goodEmail <- randomEmail <&> \e -> e {emailDomain = "good.domain.com"}
  post (brig . path "/activate/send" . contentJson . body (p goodEmail)) !!! do
    const 200 === statusCode
  where
    p email = RequestBodyLBS . encode $ SendActivationCode email Nothing

-- | FUTUREWORK: @setRestrictUserCreation@ perhaps needs to be tested in one place only, since it's the
-- first thing that we check on the /register endpoint. Other tests that make use of @setRestrictUserCreation@
-- can probably be removed and simplified. It's probably a good candidate for Quickcheck.
testRestrictedUserCreation :: Opt.Opts -> Brig -> Http ()
testRestrictedUserCreation opts brig = do
  -- We create a team before to help in other tests
  (teamOwner, createdTeam) <- createUserWithTeam brig

  let opts' = opts {Opt.optSettings = (Opt.optSettings opts) {Opt.setRestrictUserCreation = Just True}}
  withSettingsOverrides opts' $ do
    e <- randomEmail

    let Object regularUser =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "email_code" .= ("123456" :: Text),
              "password" .= plainTextPassword6Unsafe "123123123"
            ]
    postUserRegister' regularUser brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    let Object regularUserNotPreActivated =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "password" .= plainTextPassword6Unsafe "123123123"
            ]
    postUserRegister' regularUserNotPreActivated brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    let Object teamCreator =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "email_code" .= ("123456" :: Text),
              "team" .= object ["name" .= ("Alice team" :: Text), "icon" .= ("default" :: Text), "binding" .= True],
              "password" .= plainTextPassword6Unsafe "123123123"
            ]
    postUserRegister' teamCreator brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    -- Ensure you can invite team users
    void $ inviteAndRegisterUser teamOwner createdTeam brig

    -- Ephemeral users can always be created (expires_in is OPTIONAL)
    let Object ephemeralUser =
          object
            [ "name" .= Name "Alice",
              "expires_in" .= (600000 :: Int)
            ]
    postUserRegister' ephemeralUser brig !!! const 201 === statusCode

    -- Ephemeral users can always be created (expires_in is OPTIONAL and
    -- used for instance when creating guestrooms
    let Object ephemeralUserWithoutExpires =
          object
            [ "name" .= Name "Alice"
            ]
    postUserRegister' ephemeralUserWithoutExpires brig !!! const 201 === statusCode

    -- NOTE: SSO users are anyway not allowed on the `/register` endpoint
    teamid <- Id <$> liftIO UUID.nextRandom
    let ssoid = UserSSOId mkSimpleSampleUref
    let Object ssoUser =
          object
            [ "name" .= Name "Alice",
              "sso_id" .= Just ssoid,
              "team_id" .= Just teamid
            ]
    postUserRegister' ssoUser brig !!! const 400 === statusCode

-- | FUTUREWORK: @setRestrictUserCreation@ perhaps needs to be tested in one place only, since it's the
-- first thing that we check on the /register endpoint. Other tests that make use of @setRestrictUserCreation@
-- can probably be removed and simplified. It's probably a good candidate for Quickcheck.
testTooManyMembersForLegalhold :: Opt.Opts -> Brig -> Http ()
testTooManyMembersForLegalhold opts brig = do
  (owner, tid) <- createUserWithTeam brig

  -- Invite a user with mocked galley which tells us that the user cannot be
  -- added. We cannot use real galley here as the real galley has legalhold set
  -- to "whitelist-teams-and-implicit-consent". In this mode this error is not
  -- thrown, so in order to emulate other modes, we just emulate what galley
  -- would return in that case.
  inviteeEmail <- randomEmail
  let invite = stdInvitationRequest inviteeEmail
  inv <-
    responseJsonError
      =<< postInvitation brig tid owner invite
        <!! statusCode === const 201
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  let mockGalley (ReceivedRequest mth pth _body)
        | mth == "GET" && pth == ["i", "teams", Text.pack (show tid), "members", "check"] =
            pure . Wai.responseLBS HTTP.status403 mempty $
              encode
                ( Wai.mkError
                    HTTP.status403
                    "too-many-members-for-legalhold"
                    "cannot add more members to team when legalhold service is enabled."
                )
        | mth == "GET"
            && pth == ["i", "teams", Text.pack (show tid), "features", "exposeInvitationURLsToTeamAdmin"] =
            pure . Wai.responseLBS HTTP.status200 mempty $
              encode
                ( withStatus
                    FeatureStatusDisabled
                    LockStatusLocked
                    ExposeInvitationURLsToTeamAdminConfig
                    FeatureTTLUnlimited
                )
        | otherwise = pure $ Wai.responseLBS HTTP.status500 mempty "Unexpected request to mocked galley"

  void . withMockedGalley opts mockGalley $ do
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      !!! do
        const 403 === statusCode
        const (Right "too-many-members-for-legalhold") === fmap Wai.label . responseJsonEither

testDeleteUserWithCompletelyDeletedUser :: Brig -> Cannon -> UserJournalWatcher -> Http ()
testDeleteUserWithCompletelyDeletedUser brig cannon userJournalWatcher = do
  u <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher u "user activate testDeleteUserWithCompletelyDeletedUser"
  setHandleAndDeleteUser brig cannon u [] userJournalWatcher $
    \uid -> deleteUserInternal uid brig !!! const 202 === statusCode
  do
    let uid = userId u
    deleteUserInternal uid brig
      !!! do
        const 200 === statusCode

testDeleteUserWithNoUser :: Brig -> Http ()
testDeleteUserWithNoUser brig = do
  nonExistingUid :: UserId <- liftIO $ generate arbitrary
  deleteUserInternal nonExistingUid brig
    !!! do
      const 404 === statusCode

testDeleteUserWithNotDeletedUser :: (HasCallStack) => Brig -> Cannon -> UserJournalWatcher -> Http ()
testDeleteUserWithNotDeletedUser brig cannon userJournalWatcher = do
  u <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher u "user activate testDeleteUserWithNotDeletedUser"
  do
    setHandleAndDeleteUser brig cannon u [] userJournalWatcher $
      ( \uid' ->
          deleteUserInternal uid' brig
            !!! do
              const 202 === statusCode
      )

testDeleteUserWithDanglingProperty :: Brig -> Cannon -> UserJournalWatcher -> Http ()
testDeleteUserWithDanglingProperty brig cannon userJournalWatcher = do
  u <- randomUser brig
  Util.assertUserActivateJournaled userJournalWatcher u "user activate testDeleteUserWithDanglingProperty"

  let uid = userId u
  -- First set a unique handle (to verify freeing of the handle)
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode

  deleteUserInternal uid brig !!! const 202 === statusCode
  Util.assertDeleteJournaled userJournalWatcher uid "user deletion testDeleteUserWithDanglingProperty"

  setProperty brig (userId u) "foo" objectProp
    !!! const 200 === statusCode
  getProperty brig (userId u) "foo" !!! do
    const 200 === statusCode
    const (Just objectProp) === responseJsonMaybe

  execAndAssertUserDeletion brig cannon u (fromJust (parseHandle hdl)) [] userJournalWatcher $ \uid' -> do
    deleteUserInternal uid' brig
      !!! do
        const 202 === statusCode

  getProperty brig (userId u) "foo" !!! do
    const 404 === statusCode
  where
    objectProp =
      object
        [ "key.1" .= ("val1" :: Text),
          "key.2" .= ("val2" :: Text)
        ]

-- helpers

setHandleAndDeleteUser :: Brig -> Cannon -> User -> [UserId] -> UserJournalWatcher -> (UserId -> HttpT IO ()) -> Http ()
setHandleAndDeleteUser brig cannon u others userJournalWatcher execDelete = do
  let uid = userId u
  -- First set a unique handle (to verify freeing of the handle)
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode

  execAndAssertUserDeletion brig cannon u (fromJust (parseHandle hdl)) others userJournalWatcher execDelete

execAndAssertUserDeletion :: Brig -> Cannon -> User -> Handle -> [UserId] -> UserJournalWatcher -> (UserId -> HttpT IO ()) -> Http ()
execAndAssertUserDeletion brig cannon u hdl others userJournalWatcher execDelete = do
  let uid = userId u
      quid = userQualifiedId u
      email = fromMaybe (error "Must have an email set") (userEmail u)

  -- Delete the user
  WS.bracketRN cannon (uid : others) $ \wss -> do
    execDelete uid
    void . liftIO . WS.assertMatchN (5 # Second) wss $ matchDeleteUserNotification quid
    Util.assertDeleteJournaled userJournalWatcher uid "user deletion, setHandleAndDeleteUser: "
  -- Cookies are gone
  n2 <- countCookies brig uid defCookieLabel
  liftIO $ Just 0 @=? n2
  -- Clients are gone
  get (brig . path "clients" . zUser (userId u)) !!! do
    const 200 === statusCode
    const (Just [] :: Maybe [Client]) === responseJsonMaybe
  -- Can no longer log in
  login brig (defEmailLogin email) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === fmap Error.label . responseJsonMaybe
  -- Deleted flag appears in self profile; email, handle and picture are gone
  get (brig . path "/self" . zUser uid) !!! assertDeletedProfileSelf
  Search.refreshIndex brig
  -- Does not appear in search; public profile shows the user as deleted
  forM_ others $ \usr -> do
    get (apiVersion "v1" . brig . paths ["users", toByteString' uid] . zUser usr) !!! assertDeletedProfilePublic
    Search.assertCan'tFind brig usr quid (fromName (userDisplayName u))
    Search.assertCan'tFind brig usr quid (fromHandle hdl)
  -- Email address is available again
  let Object o =
        object
          [ "name" .= ("Someone Else" :: Text),
            "email" .= fromEmail email,
            "password" .= defPassword
          ]
  -- This will generate a new event, we need to consume it here
  usr <- postUserInternal o brig
  Util.assertUserActivateJournaled userJournalWatcher usr "user activate execAndAssertUserDeletion"
  -- Handle is available again
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 404 === statusCode
  where
    assertDeletedProfileSelf = do
      const 200 === statusCode
      const (Just noPict, Just Nothing, Just True, Just [], Nothing)
        === ( \u' ->
                ( fmap userPict u',
                  fmap userEmail u',
                  fmap userDeleted u',
                  fmap userAssets u',
                  userHandle =<< u'
                )
            )
          . responseJsonMaybe
    assertDeletedProfilePublic = do
      const 200 === statusCode
      const (Just noPict, Just True, Nothing)
        === ( \u' ->
                ( fmap profilePict u',
                  fmap profileDeleted u',
                  profileHandle =<< u'
                )
            )
          . responseJsonMaybe
