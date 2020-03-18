module API.Settings where

import API.Team.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Options (Opts)
import qualified Brig.Options as Opt
import Brig.Run (mkApp)
import Brig.Types (Email (..), User (..), userEmail)
import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Id
import qualified Data.Set as Set
import qualified Galley.Types.Teams as Team
import Imports
import qualified Network.Wai.Test as WaiTest
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util

withCustomOptions :: Opts -> WaiTest.Session a -> IO a
withCustomOptions opts sess = do
  (app, _) <- mkApp opts
  WaiTest.runSession sess app

tests :: Opts -> Manager -> Brig -> Galley -> IO TestTree
tests defOpts manager brig galley = return $ do
  testGroup
    "settings"
    [ testGroup
        "setEmailVisibility"
        [ testGroup
            "/users/"
            $ ((,) <$> [minBound ..] <*> [minBound ..])
              <&> \(viewingUserIs, visibility) -> do
                testCase (show (viewingUserIs, visibility))
                  . runHttpT manager
                  $ testUsersEmailVisibleIffExpected defOpts brig galley viewingUserIs visibility,
          testGroup
            "/users/:uid"
            $ ((,) <$> [minBound ..] <*> [minBound ..])
              <&> \(viewingUserIs, visibility) -> do
                testCase (show (viewingUserIs, visibility))
                  . runHttpT manager
                  $ testGetUserEmailShowsEmailsIffExpected defOpts brig galley viewingUserIs visibility
        ]
    ]

-- | The user looking at users is always a team creator; the user looked falls into the
-- different categories enumerated here.
data ViewedUserIs = SameTeam | DifferentTeam | NoTeam

-- | Analog of 'ViewedUserIs' for the viewing user.
data ViewingUserIs = Creator | Member | Guest
  deriving (Eq, Show, Enum, Bounded)

expectEmailVisible :: Opt.EmailVisibility -> ViewingUserIs -> ViewedUserIs -> Bool
expectEmailVisible Opt.EmailVisibleIfOnTeam = \case
  _ -> \case
    SameTeam -> True
    DifferentTeam -> True
    NoTeam -> False
expectEmailVisible Opt.EmailVisibleIfOnSameTeam = \case
  Creator -> \case
    SameTeam -> True
    DifferentTeam -> False
    NoTeam -> False
  Member -> \case
    SameTeam -> True
    DifferentTeam -> False
    NoTeam -> False
  Guest -> \case
    SameTeam -> False
    DifferentTeam -> False
    NoTeam -> False
expectEmailVisible Opt.EmailVisibleToSelf = \case
  _ -> \case
    SameTeam -> False
    DifferentTeam -> False
    NoTeam -> False

jsonField :: FromJSON a => Text -> Value -> Maybe a
jsonField f u = u ^? key f >>= maybeFromJSON

testUsersEmailVisibleIffExpected :: Opts -> Brig -> Galley -> ViewingUserIs -> Opt.EmailVisibility -> Http ()
testUsersEmailVisibleIffExpected opts brig galley viewingUserIs visibilitySetting = do
  (viewerId, userA, userB, nonTeamUser) <- setup brig galley viewingUserIs
  let uids =
        C8.intercalate "," $
          toByteString' <$> [userId userA, userId userB, userId nonTeamUser]
      expected :: Set (Maybe UserId, Maybe Email)
      expected =
        Set.fromList
          [ ( Just $ userId userA,
              if expectEmailVisible visibilitySetting viewingUserIs SameTeam
                then userEmail userA
                else Nothing
            ),
            ( Just $ userId userB,
              if expectEmailVisible visibilitySetting viewingUserIs DifferentTeam
                then userEmail userB
                else Nothing
            ),
            ( Just $ userId nonTeamUser,
              if expectEmailVisible visibilitySetting viewingUserIs NoTeam
                then userEmail nonTeamUser
                else Nothing
            )
          ]
  let newOpts = opts & Opt.optionSettings . Opt.emailVisibility .~ visibilitySetting
  withSettingsOverrides newOpts $ do
    get (brig . zUser viewerId . path "users" . queryItem "ids" uids) !!! do
      const 200 === statusCode
      const (Just expected) === result
  where
    result r = Set.fromList . map (jsonField "id" &&& jsonField "email") <$> responseJsonMaybe r

testGetUserEmailShowsEmailsIffExpected :: Opts -> Brig -> Galley -> ViewingUserIs -> Opt.EmailVisibility -> Http ()
testGetUserEmailShowsEmailsIffExpected opts brig galley viewingUserIs visibilitySetting = do
  (viewerId, userA, userB, nonTeamUser) <- setup brig galley viewingUserIs
  let expectations :: [(UserId, Maybe Email)]
      expectations =
        [ ( userId userA,
            if expectEmailVisible visibilitySetting viewingUserIs SameTeam
              then userEmail userA
              else Nothing
          ),
          ( userId userB,
            if expectEmailVisible visibilitySetting viewingUserIs DifferentTeam
              then userEmail userB
              else Nothing
          ),
          ( userId nonTeamUser,
            if expectEmailVisible visibilitySetting viewingUserIs NoTeam
              then userEmail nonTeamUser
              else Nothing
          )
        ]
  let newOpts = opts & Opt.optionSettings . Opt.emailVisibility .~ visibilitySetting
  withSettingsOverrides newOpts $ do
    forM_ expectations $ \(uid, expectedEmail) ->
      get (brig . zUser viewerId . paths ["users", toByteString' uid]) !!! do
        const 200 === statusCode
        const expectedEmail === emailResult
  where
    emailResult :: Response (Maybe LByteString) -> Maybe Email
    emailResult r = responseJsonMaybe r >>= jsonField "email"

setup :: Brig -> Galley -> ViewingUserIs -> Http (UserId, User, User, User)
setup brig galley viewingUserIs = do
  (creatorId, tid) <- createUserWithTeam brig galley
  (otherTeamCreatorId, otherTid) <- createUserWithTeam brig galley
  userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
  userB <- createTeamMember brig galley otherTeamCreatorId otherTid Team.fullPermissions
  nonTeamUser <- createUser "joe" brig
  viewerId <- case viewingUserIs of
    Creator -> pure creatorId
    Member -> userId <$> createTeamMember brig galley creatorId tid (Team.rolePermissions Team.RoleOwner)
    Guest -> userId <$> createTeamMember brig galley creatorId tid (Team.rolePermissions Team.RoleExternalPartner)
  pure (viewerId, userA, userB, nonTeamUser)
