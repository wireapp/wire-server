{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-unused-local-binds #-}
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

module API.UserPendingActivation where

import API.Team.Util (getTeams)
-- import Web.HttpApiData (toHeader)

import Bilge (Http, MonadHttp, Response (responseBody), post, responseJsonUnsafe)
import Bilge.Assert ((<!!), (===))
import Bilge.IO (Manager)
import qualified Bilge.IO as Bilge
import Bilge.Request
import Bilge.Response (ResponseLBS, statusCode)
import Brig.Types
import qualified Brig.Types as Brig
import Brig.Types.Team.Invitation (Invitation (inInvitation))
import Cassandra
import qualified Control.Exception
import Control.Lens ((^.), (^?))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Random
import Data.Aeson hiding (json)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Conversion (fromByteString, toByteString')
import Data.Id (InvitationId, TeamId, UserId)
import Data.Range (unsafeRange)
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Galley.Types.Teams as Galley
import Imports
import qualified SAML2.WebSSO as SAML
import Spar.Scim (CreateScimTokenResponse (..), SparTag, userSchemas)
import Spar.Scim.Types (CreateScimToken (..), ScimUserExtra (ScimUserExtra))
import Spar.Types
import Test.QuickCheck (generate)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.Tasty
import qualified Text.Email.Parser as Email
import Util hiding (createUser)
import Web.HttpApiData (toHeader)
import qualified Web.Scim.Class.User as Scim
import qualified Web.Scim.Schema.User as Scim.User
import qualified Web.Scim.Schema.User.Email as Email
import qualified Web.Scim.Schema.User.Phone as Phone
import Wire.API.User.RichInfo (RichInfo)

-- import SAML2.WebSSO.Types

-- import qualified Web.Scim.Schema.User as Scim.User

tests :: Manager -> ClientState -> Brig -> Galley -> Spar -> IO TestTree
tests m db brig galley spar = do
  return $
    testGroup
      "cleanExpiredPendingInvitations"
      [test m "works" (testCleanExpiredPendingInvitations db brig galley spar)]

testCleanExpiredPendingInvitations :: ClientState -> Brig -> Galley -> Spar -> Http ()
testCleanExpiredPendingInvitations _db brig galley spar = do
  email <- randomEmail
  (owner, tid) <- createUserWithTeamDisableSSO brig galley
  CreateScimTokenResponse tok _ <-
    createToken spar owner $
      CreateScimToken
        { createScimTokenDescr = "testCreateToken",
          createScimTokenPassword = Just defPassword
        }
  scimUser <- lift (randomScimUser <&> \u -> u {Scim.User.externalId = Just $ fromEmail email})
  (scimStoredUser1, _inv, inviteeCode) <- createUser'step spar brig tok tid scimUser email
  print scimStoredUser1
  print inviteeCode
  where
    createUser'step spar' brig' tok tid scimUser email = do
      -- scimStoredUser <- aFewTimesRecover (createUser tok scimUser)
      scimStoredUser <- (createUser spar' tok scimUser)
      inv <- getInvitationByEmail brig' email
      Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
      pure (scimStoredUser, inv, inviteeCode)

getInvitationByEmail :: Brig -> Email -> Http Invitation
getInvitationByEmail brig email =
  responseJsonUnsafe
    <$> ( Bilge.get (brig . path "/i/teams/invitations/by-email" . contentJson . queryItem "email" (toByteString' email))
            <!! const 200 === statusCode
        )

newTeam :: Galley.BindingNewTeam
newTeam = Galley.BindingNewTeam $ Galley.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")

createUserWithTeamDisableSSO :: (HasCallStack, MonadCatch m, MonadHttp m, MonadIO m, MonadFail m) => Brig -> Galley -> m (UserId, TeamId)
createUserWithTeamDisableSSO brg gly = do
  e <- randomEmail
  n <- UUID.toString <$> liftIO UUID.nextRandom
  let p =
        RequestBodyLBS . Aeson.encode $
          object
            [ "name" .= n,
              "email" .= Brig.fromEmail e,
              "password" .= defPassword,
              "team" .= newTeam
            ]
  bdy <- selfUser . responseJsonUnsafe <$> post (brg . path "/i/users" . contentJson . body p)
  let (uid, Just tid) = (Brig.userId bdy, Brig.userTeam bdy)
  (team : _) <- (^. Galley.teamListTeams) <$> getTeams uid gly
  () <-
    Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid == team ^. Galley.teamId) $
      pure ()
  selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
  () <-
    Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid) $
      pure ()
  return (uid, tid)

randomScimUser :: (HasCallStack, MonadRandom m, MonadIO m) => m (Scim.User.User SparTag)
randomScimUser = fst <$> randomScimUserWithSubject

-- | Like 'randomScimUser', but also returns the intended subject ID that the user should
-- have. It's already available as 'Scim.User.externalId' but it's not structured.
randomScimUserWithSubject ::
  (HasCallStack, MonadRandom m, MonadIO m) =>
  m (Scim.User.User SparTag, SAML.UnqualifiedNameID)
randomScimUserWithSubject = do
  randomScimUserWithSubjectAndRichInfo =<< liftIO (generate arbitrary)

-- | See 'randomScimUser', 'randomScimUserWithSubject'.
randomScimUserWithSubjectAndRichInfo ::
  (HasCallStack, MonadRandom m, MonadIO m) =>
  RichInfo ->
  m (Scim.User.User SparTag, SAML.UnqualifiedNameID)
randomScimUserWithSubjectAndRichInfo richInfo = do
  suffix <- cs <$> replicateM 7 (getRandomR ('0', '9'))
  emails <- getRandomR (0, 3) >>= \n -> replicateM n randomScimEmail
  phones <- getRandomR (0, 3) >>= \n -> replicateM n randomScimPhone
  -- Related, but non-trivial to re-use here: 'nextSubject'
  (externalId, subj) <-
    getRandomR (0, 1 :: Int) <&> \case
      0 ->
        ( "scimuser_extid_" <> suffix <> "@example.com",
          either (error . show) id $
            SAML.mkUNameIDEmail ("scimuser_extid_" <> suffix <> "@example.com")
        )
      1 ->
        ( "scimuser_extid_" <> suffix,
          SAML.mkUNameIDUnspecified ("scimuser_extid_" <> suffix)
        )
      _ -> error "randomScimUserWithSubject: impossible"
  pure
    ( (Scim.User.empty userSchemas ("scimuser_" <> suffix) (ScimUserExtra richInfo))
        { Scim.User.displayName = Just ("ScimUser" <> suffix),
          Scim.User.externalId = Just externalId,
          Scim.User.emails = emails,
          Scim.User.phoneNumbers = phones
        },
      subj
    )

randomScimEmail :: MonadRandom m => m Email.Email
randomScimEmail = do
  let typ :: Maybe Text = Nothing
      primary :: Maybe Bool = Nothing -- TODO: where should we catch users with more than one
      -- primary email?
  value :: Email.EmailAddress2 <- do
    localpart <- cs <$> replicateM 15 (getRandomR ('a', 'z'))
    domainpart <- (<> ".com") . cs <$> replicateM 15 (getRandomR ('a', 'z'))
    pure . Email.EmailAddress2 $ Email.unsafeEmailAddress localpart domainpart
  pure Email.Email {..}

randomScimPhone :: MonadRandom m => m Phone.Phone
randomScimPhone = do
  let typ :: Maybe Text = Nothing
  value :: Maybe Text <- do
    let mkdigits n = replicateM n (getRandomR ('0', '9'))
    mini <- mkdigits 8
    maxi <- mkdigits =<< getRandomR (0, 7)
    pure $ Just (cs ('+' : mini <> maxi))
  pure Phone.Phone {..}

-- | Create a user.
createUser ::
  HasCallStack =>
  Spar ->
  ScimToken ->
  Scim.User.User SparTag ->
  Http (Scim.StoredUser SparTag)
createUser spar tok user = do
  r <-
    createUser_ spar (Just tok) user <!! do
      const 201 === statusCode
  pure (responseJsonUnsafe r)

-- | Create a user.
createUser_ ::
  Spar ->
  -- | Authentication
  Maybe ScimToken ->
  -- | User data
  Scim.User.User SparTag ->
  -- | Spar endpoint
  Http ResponseLBS
createUser_ spar auth user = do
  -- NB: we don't use 'mkEmailRandomLocalSuffix' here, because emails
  -- shouldn't be submitted via SCIM anyway.
  -- TODO: what's the consequence of this?  why not update emails via
  -- SCIM?  how else should they be submitted?  i think this there is
  -- still some confusion here about the distinction between *validated*
  -- emails and *scim-provided* emails, which are two entirely
  -- different things.
  post $
    ( spar
        . paths ["scim", "v2", "Users"]
        . scimAuth auth
        . contentScim
        . json user
        . acceptScim
    )

-- | Add SCIM authentication to a request.
scimAuth :: Maybe ScimToken -> Request -> Request
scimAuth Nothing = id
scimAuth (Just auth) = header "Authorization" (toHeader auth)

-- | Signal that the body is an SCIM payload.
contentScim :: Request -> Request
contentScim = content "application/scim+json"

-- | Signal that the response type is expected to be an SCIM payload.
acceptScim :: Request -> Request
acceptScim = accept "application/scim+json"

-- -- | Get ID of a user returned from SCIM.
-- scimUserId :: Scim.StoredUser SparTag -> UserId
-- scimUserId = Scim.id . Scim.thing

getInvitationCode ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  TeamId ->
  InvitationId ->
  m (Maybe InvitationCode)
getInvitationCode brig t ref = do
  r <-
    Bilge.get
      ( brig
          . path "/i/teams/invitation-code"
          . queryItem "team" (toByteString' t)
          . queryItem "invitation_id" (toByteString' ref)
      )
  let lbs = fromMaybe "" $ responseBody r
  return $ fromByteString . fromMaybe (error "No code?") $ encodeUtf8 <$> (lbs ^? key "code" . _String)

-- | Create a SCIM token.
createToken_ ::
  Spar ->
  -- | User
  UserId ->
  CreateScimToken ->
  -- | Spar endpoint
  Http ResponseLBS
createToken_ spar userid payload = do
  post $
    ( spar
        . paths ["scim", "auth-tokens"]
        . zUser userid
        . contentJson
        . json payload
        . acceptJson
    )

-- | Create a SCIM token.
createToken ::
  HasCallStack =>
  Spar ->
  UserId ->
  CreateScimToken ->
  Http CreateScimTokenResponse
createToken spar zusr payload = do
  r <-
    createToken_
      spar
      zusr
      payload
      <!! const 200 === statusCode
  pure (responseJsonUnsafe r)
