module API.UserPendingActivation where

-- import Brig.Types
-- import Brig.Types.Intra
-- import Brig.Types.Team.Invitation
-- import Brig.Types.User.Auth

-- import API.Team.Util
-- import Bilge hiding (accept, timeout)
-- import Bilge.Assert
-- import Brig.Options (Opts)
-- import qualified Brig.Options as Opt
-- import Brig.Types (Email (..), User (..), userEmail)
-- import Control.Arrow ((&&&))
-- import Control.Lens
-- import Data.Aeson
-- import Data.Aeson.Lens
-- import qualified Data.ByteString.Char8 as C8
-- import Data.ByteString.Conversion
-- import Data.Id
-- import qualified Data.Set as Set
-- import qualified Galley.Types.Teams as Team

-- import Test.Tasty hiding (Timeout)
-- import Test.Tasty.HUnit

import API.Team.Util (getTeams)
import Bilge (MonadHttp, post, responseJsonUnsafe)
import Bilge.Request
import Brig.Types
import qualified Brig.Types as Brig
import Cassandra
import Control.Lens ((^.))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Id (TeamId, UserId)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Galley.Types.Teams (newTeam)
import qualified Galley.Types.Teams as Galley
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util

tests :: ClientState -> Brig -> Galley -> IO TestTree
tests db brig galley = do
  return $
    testGroup
      "cleanExpiredPendingInvitations"
      [testCase "works" (testCleanExpiredPendingInvitations db brig galley)]

testCleanExpiredPendingInvitations :: ClientState -> Brig -> Galley -> IO ()
testCleanExpiredPendingInvitations _db brig galley = do
  (uid, tid) <- createUserWithTeamDisableSSO brig galley
  pure ()

-- (owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)

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
  return (uid, tid)

-- let (uid, Just tid) = (Brig.userId bdy, Brig.userTeam bdy)
-- (team : _) <- (^. Galley.teamListTeams) <$> getTeams uid gly
-- () <-
--   Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid == team ^. Galley.teamId) $
--     pure ()
-- selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
-- () <-
--   Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid) $
--     pure ()
-- return (uid, tid)

-- createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m, MonadFail m) => BrigReq -> GalleyReq -> m (UserId, TeamId)
-- createUserWithTeam brg gly = do
--   (uid, tid) <- createUserWithTeamDisableSSO brg gly
--   putSSOEnabledInternal gly tid TeamFeatureEnabled
--   pure (uid, tid)

-- email <- randomEmail

-- scimUser <- randomScimUser <&> \u -> u {Scim.User.externalId = Just $ fromEmail email}
-- (owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)

-- pure ()

-- scimUser <- randomScimUser <&> \u -> u {Scim.User.externalId = Just $ fromEmail email}
-- (owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)

-- 1. get scim token and call spar
-- 2. create

-- 1. call
-- post "/i/teams/:tid/invitations" (continue createInvitationViaScimH) $
--   accept "application" "json"
--     .&. jsonRequest @NewUserScimInvitation

-- 2. get check db for users
-- 3. wait for time out
-- 4. check db for user
