module API.Settings (tests) where

import           Imports
import           Bilge               hiding (accept, timeout)
import           Data.Barbie         (buniq)
import           Test.Tasty          hiding (Timeout)
import           Util
import API.Team.Util
import qualified Galley.Types.Teams          as Team

import Data.Id
import qualified Brig.Options as Opt
import           Brig.Types.User (EmailVisibility(..))

import Test.Tasty.HUnit

import Bilge.Assert
import Brig.Types
import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.ByteString.Conversion

import qualified Data.ByteString.Char8       as C8
import qualified Data.Set                    as Set

tests :: Manager -> Brig -> Galley -> IO TestTree
tests manager brig galley = do
    return
        $ testGroup "settings"
        $ [ testGroup "EmailVisibleToAllTeams"
                      [ testCase "" . void
                            $ runHttpT manager
                                       (withEmailVisibility EmailVisibleToAllTeams
                                                            brig
                                                            (testEmailVisibleTeam brig galley))
                      ]
          , testGroup "EmailVisibleToSameTeam" []
          , testGroup "EmailVisibleToSelf" []
          ]

testEmailVisibleTeam :: Brig -> Galley -> Http ()
testEmailVisibleTeam brig galley = do
    (creatorId, tid) <- createUserWithTeam brig galley
    userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
    userB <- createTeamMember brig galley creatorId tid Team.fullPermissions
    let uids = C8.intercalate "," $ toByteString' <$> [userId userA, userId userB]
        expected :: Set (Maybe UserId, Maybe Email)
        expected = Set.fromList
                   [ (Just $ userId userA, userEmail userA)
                   , (Just $ userId userB, userEmail userB)
                   ]
    get (brig . zUser (userId userB) . path "users" . queryItem "ids" uids) !!! do
        const 200 === statusCode
        const (Just expected) === result
  where
    result r =  Set.fromList
             .  map (field "id" &&& field "email")
            <$> decodeBody r

    field :: FromJSON a => Text -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON


withEmailVisibility :: EmailVisibility -> Brig -> Http () -> Http ()
withEmailVisibility emailVisibilityOverride brig t =
    withSettingsOverrides brig newSettings t
  where
    newSettings :: Opt.MutableSettings' Maybe
    newSettings =
        (buniq Nothing)
        { Opt.setEmailVisibility = Just emailVisibilityOverride
        }
