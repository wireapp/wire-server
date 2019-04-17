module API.Settings  where

import           Imports
import           Bilge              hiding (accept, timeout)
import           Brig.Options (Opts)
import           Brig.Run (mkApp)
import           Data.Barbie        (buniq)
import           Test.Tasty         hiding (Timeout)
import           Util
import           API.Team.Util
import qualified Galley.Types.Teams as Team

import Data.Id
import qualified Brig.Options as Opt

-- import Test.Tasty.HUnit

import Bilge.Assert
import Brig.Types
import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.ByteString.Conversion

import qualified Network.Wai.Test as WaiTest

import qualified Data.ByteString.Char8       as C8
import qualified Data.Set                    as Set

withCustomOptions :: Opts -> WaiTest.Session a -> IO a
withCustomOptions opts sess = do
    (app, _) <- mkApp opts
    WaiTest.runSession sess app

tests :: Opts -> Manager -> Brig -> Galley -> IO TestTree
tests _defOpts _manager _brig _galley = return $ do
    testGroup "settings"
        [ testGroup "setEmailVisibility"
            [ -- testCase "testThing" . runHttpT manager $ testThing defOpts brig galley (expectEmailVisible Opt.EmailVisibleIfOnTeam)

            ]
            -- [ testGroup "/users/"
            --     [ testCase "EmailVisibleIfOnTeam"
            --     . runHttpT manager
            --     . withEmailVisibility Opt.EmailVisibleIfOnTeam brig
            --     $ testUsersEmailShowsEmailsIfExpected brig galley (expectEmailVisible Opt.EmailVisibleIfOnTeam)

            --     , testCase "EmailVisibleToSelf"
            --     . runHttpT manager
            --     . withEmailVisibility Opt.EmailVisibleToSelf brig
            --     $ testUsersEmailShowsEmailsIfExpected brig galley (expectEmailVisible Opt.EmailVisibleToSelf)
            --     ]
            -- , testGroup "/users/:id"
            --     [ testCase "EmailVisibleIfOnTeam"
            --     . runHttpT manager
            --     . withEmailVisibility Opt.EmailVisibleIfOnTeam brig
            --     $ testGetUserEmailShowsEmailsIfExpected brig galley (expectEmailVisible Opt.EmailVisibleIfOnTeam)

            --     , testCase "EmailVisibleToSelf"
            --     . runHttpT manager
            --     . withEmailVisibility Opt.EmailVisibleToSelf brig
            --     $ testGetUserEmailShowsEmailsIfExpected brig galley (expectEmailVisible Opt.EmailVisibleToSelf)
            --     ]
            -- ]
        ]

data UserRelationship = SameTeam | DifferentTeam | NoTeam

-- Should we show the email for this user type?
type EmailVisibilityAssertion = UserRelationship -> Bool

expectEmailVisible :: Opt.EmailVisibility -> UserRelationship -> Bool
expectEmailVisible Opt.EmailVisibleIfOnTeam SameTeam = True
expectEmailVisible Opt.EmailVisibleIfOnTeam DifferentTeam = True
expectEmailVisible Opt.EmailVisibleIfOnTeam NoTeam = False

expectEmailVisible Opt.EmailVisibleToSelf SameTeam = False
expectEmailVisible Opt.EmailVisibleToSelf DifferentTeam = False
expectEmailVisible Opt.EmailVisibleToSelf NoTeam = False

jsonField :: FromJSON a => Text -> Value -> Maybe a
jsonField f u = u ^? key f >>= maybeFromJSON

testThing :: Opts -> Brig -> Galley -> EmailVisibilityAssertion -> Http ()
testThing _opts brig galley shouldShowEmail = do
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


withEmailVisibility :: Opt.EmailVisibility -> Brig -> Http () -> Http ()
withEmailVisibility emailVisibilityOverride brig t =
    withSettingsOverrides brig newSettings t
  where
    newSettings :: Opt.MutableSettings' Maybe
    newSettings =
        (buniq Nothing)
        { Opt.setEmailVisibility = Just emailVisibilityOverride
        }
