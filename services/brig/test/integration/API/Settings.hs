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
tests manager brig galley = return $ do
    testGroup "settings"
        [ testGroup "setEmailVisibility"
            [ testGroup "/users/"
                [ testCase "EmailVisibleIfOnTeam"
                . runHttpT manager
                . withEmailVisibility EmailVisibleIfOnTeam brig
                $ testUsersEmailShowsEmailsIfExpected brig galley (expectEmailVisible EmailVisibleIfOnTeam)

                , testCase "EmailVisibleToSelf"
                . runHttpT manager
                . withEmailVisibility EmailVisibleToSelf brig
                $ testUsersEmailShowsEmailsIfExpected brig galley (expectEmailVisible EmailVisibleToSelf)
                ]
            , testGroup "/users/:id"
                [ testCase "EmailVisibleIfOnTeam"
                . runHttpT manager
                . withEmailVisibility EmailVisibleIfOnTeam brig
                $ testGetUserEmailShowsEmailsIfExpected brig galley (expectEmailVisible EmailVisibleIfOnTeam)

                , testCase "EmailVisibleToSelf"
                . runHttpT manager
                . withEmailVisibility EmailVisibleToSelf brig
                $ testGetUserEmailShowsEmailsIfExpected brig galley (expectEmailVisible EmailVisibleToSelf)
                ]
            ]
        ]

data UserRelationship = SameTeam | DifferentTeam | NoTeam

-- Should we show the email for this user type?
type EmailVisibilityAssertion = UserRelationship -> Bool

expectEmailVisible :: EmailVisibility -> UserRelationship -> Bool
expectEmailVisible EmailVisibleIfOnTeam SameTeam = True
expectEmailVisible EmailVisibleIfOnTeam DifferentTeam = True
expectEmailVisible EmailVisibleIfOnTeam NoTeam = False

expectEmailVisible EmailVisibleToSelf SameTeam = False
expectEmailVisible EmailVisibleToSelf DifferentTeam = False
expectEmailVisible EmailVisibleToSelf NoTeam = False

jsonField :: FromJSON a => Text -> Value -> Maybe a
jsonField f u = u ^? key f >>= maybeFromJSON

testUsersEmailShowsEmailsIfExpected :: Brig -> Galley -> EmailVisibilityAssertion -> Http ()
testUsersEmailShowsEmailsIfExpected brig galley shouldShowEmail = do
    (creatorId, tid) <- createUserWithTeam brig galley
    (otherTeamCreatorId, otherTid) <- createUserWithTeam brig galley
    userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
    userB <- createTeamMember brig galley otherTeamCreatorId otherTid Team.fullPermissions
    nonTeamUser <- createUser "joe" brig
    let uids = C8.intercalate "," $ toByteString' <$> [userId userA, userId userB, userId nonTeamUser]
        expected :: Set (Maybe UserId, Maybe Email)
        expected = Set.fromList
                   [ ( Just $ userId userA
                     , if shouldShowEmail SameTeam then userEmail userA
                                                   else Nothing)
                   , ( Just $ userId userB
                     , if shouldShowEmail DifferentTeam then userEmail userB
                                                        else Nothing)
                   , ( Just $ userId nonTeamUser
                     , if shouldShowEmail NoTeam then userEmail nonTeamUser
                                                 else Nothing)
                   ]
    get (brig . zUser (userId userB) . path "users" . queryItem "ids" uids) !!! do
        const 200 === statusCode
        const (Just expected) === result
  where
    result r =  Set.fromList
             .  map (jsonField "id" &&& jsonField "email")
            <$> decodeBody r

testGetUserEmailShowsEmailsIfExpected :: Brig -> Galley -> EmailVisibilityAssertion -> Http ()
testGetUserEmailShowsEmailsIfExpected brig galley shouldShowEmail = do
    (creatorId, tid) <- createUserWithTeam brig galley
    (otherTeamCreatorId, otherTid) <- createUserWithTeam brig galley
    userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
    userB <- createTeamMember brig galley otherTeamCreatorId otherTid Team.fullPermissions
    nonTeamUser <- createUser "joe" brig
    let expectations :: [(UserId, Maybe Email)]
        expectations =
            [ (userId userA, if shouldShowEmail SameTeam then userEmail userA
                                                         else Nothing)
            , (userId userB, if shouldShowEmail DifferentTeam then userEmail userB
                                                              else Nothing)
            , (userId nonTeamUser, if shouldShowEmail NoTeam then userEmail nonTeamUser
                                                             else Nothing)
            ]
    forM_ expectations $ \(uid, expectedEmail) ->
        get (brig . zUser (userId userB) . paths ["users", toByteString' uid]) !!! do
            const 200 === statusCode
            const expectedEmail === emailResult
  where
    emailResult :: Response (Maybe LByteString) -> Maybe Email
    emailResult r = decodeBody r >>= jsonField "email"


withEmailVisibility :: EmailVisibility -> Brig -> Http () -> Http ()
withEmailVisibility emailVisibilityOverride brig t =
    withSettingsOverrides brig newSettings t
  where
    newSettings :: Opt.MutableSettings' Maybe
    newSettings =
        (buniq Nothing)
        { Opt.setEmailVisibility = Just emailVisibilityOverride
        }
