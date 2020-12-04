{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.UserPendingActivation where

import API.Team.Util (getTeams)
import Bilge (Http, MonadHttp, post, responseJsonUnsafe)
import Bilge.IO (Manager)
import Bilge.Request
import Brig.Types
import qualified Brig.Types as Brig
import Cassandra
import qualified Control.Exception
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Random
import Control.Monad.Random.Class (MonadRandom)
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Id (TeamId, UserId)
import Data.Range (unsafeRange)
import Data.String.Conversions (cs)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Galley.Types.Teams as Galley
import Imports
import qualified SAML2.WebSSO as SAML
import Spar.Scim (ScimUserExtra, SparTag, userSchemas)
import Spar.Scim.Types (ScimUserExtra (ScimUserExtra))
import Test.QuickCheck
import Test.Tasty
import qualified Text.Email.Parser as Email
import Util
import qualified Web.Scim.Schema.User as Scim.User
import qualified Web.Scim.Schema.User.Email as Email
import qualified Web.Scim.Schema.User.Phone as Phone
import Wire.API.User.RichInfo (RichInfo (RichInfo))

-- import SAML2.WebSSO.Types

-- import qualified Web.Scim.Schema.User as Scim.User

tests :: Manager -> ClientState -> Brig -> Galley -> IO TestTree
tests m db brig galley = do
  return $
    testGroup
      "cleanExpiredPendingInvitations"
      [test m "works" (testCleanExpiredPendingInvitations db brig galley)]

testCleanExpiredPendingInvitations :: ClientState -> Brig -> Galley -> Http ()
testCleanExpiredPendingInvitations _db brig galley = do
  _email <- randomEmail
  -- scimUser <- randomScimUser <&> \u -> u {Scim.User.externalId = Just $ fromEmail email}
  (_uid, _tid) <- createUserWithTeamDisableSSO brig galley
  pure ()

-- (owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)

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
