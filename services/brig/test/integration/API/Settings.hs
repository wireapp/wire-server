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

module API.Settings (tests) where

import API.Team.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Options (Opts)
import Brig.Options qualified as Opt
import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Conversion
import Data.Id
import Data.Set qualified as Set
import Imports
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util
import Wire.API.Team.Member (rolePermissions)
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User

allEmailVisibilities :: [EmailVisibilityConfig]
allEmailVisibilities = [EmailVisibleIfOnTeam, EmailVisibleIfOnSameTeam (), EmailVisibleToSelf]

tests :: Opts -> Manager -> Brig -> Galley -> IO TestTree
tests defOpts manager brig galley = pure $ do
  testGroup
    "settings"
    [ testGroup
        "setEmailVisibility"
        [ testGroup
            "/users/"
            $ ((,) <$> [minBound ..] <*> allEmailVisibilities)
              <&> \(viewingUserIs, visibility) -> do
                testCase (show (viewingUserIs, visibility))
                  . runHttpT manager
                  $ testUsersEmailVisibleIffExpected defOpts brig galley viewingUserIs visibility,
          testGroup
            "/users/:uid"
            $ ((,) <$> [minBound ..] <*> allEmailVisibilities)
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

expectEmailVisible :: EmailVisibilityConfig -> ViewingUserIs -> ViewedUserIs -> Bool
expectEmailVisible EmailVisibleIfOnTeam = \case
  _ -> \case
    SameTeam -> True
    DifferentTeam -> True
    NoTeam -> False
expectEmailVisible (EmailVisibleIfOnSameTeam _) = \case
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
expectEmailVisible EmailVisibleToSelf = \case
  _ -> \case
    SameTeam -> False
    DifferentTeam -> False
    NoTeam -> False

jsonField :: (FromJSON a) => Key -> Value -> Maybe a
jsonField f u = u ^? key f >>= maybeFromJSON

testUsersEmailVisibleIffExpected :: Opts -> Brig -> Galley -> ViewingUserIs -> EmailVisibilityConfig -> Http ()
testUsersEmailVisibleIffExpected opts brig galley viewingUserIs visibilitySetting = do
  (viewerId, userA, userB, nonTeamUser) <- setup brig galley viewingUserIs
  let uids =
        C8.intercalate "," $
          toByteString' <$> [userId userA, userId userB, userId nonTeamUser]
      expected :: Set (Maybe UserId, Maybe EmailAddress)
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
    get (apiVersion "v1" . brig . zUser viewerId . path "users" . queryItem "ids" uids) !!! do
      const 200 === statusCode
      const (Just expected) === result
  where
    result r = Set.fromList . map (jsonField "id" &&& jsonField "email") <$> responseJsonMaybe r

testGetUserEmailShowsEmailsIffExpected :: Opts -> Brig -> Galley -> ViewingUserIs -> EmailVisibilityConfig -> Http ()
testGetUserEmailShowsEmailsIffExpected opts brig galley viewingUserIs visibilitySetting = do
  (viewerId, userA, userB, nonTeamUser) <- setup brig galley viewingUserIs
  let expectations :: [(UserId, Maybe EmailAddress)]
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
      get (apiVersion "v1" . brig . zUser viewerId . paths ["users", toByteString' uid]) !!! do
        const 200 === statusCode
        const expectedEmail === emailResult
  where
    emailResult :: Response (Maybe LByteString) -> Maybe EmailAddress
    emailResult r = responseJsonMaybe r >>= jsonField "email"

setup :: Brig -> Galley -> ViewingUserIs -> Http (UserId, User, User, User)
setup brig galley viewingUserIs = do
  (creatorId, tid) <- createUserWithTeam brig
  (otherTeamCreatorId, otherTid) <- createUserWithTeam brig
  userA <- createTeamMember brig galley creatorId tid fullPermissions
  userB <- createTeamMember brig galley otherTeamCreatorId otherTid fullPermissions
  nonTeamUser <- createUser "joe" brig
  viewerId <- case viewingUserIs of
    Creator -> pure creatorId
    Member -> userId <$> createTeamMember brig galley creatorId tid (rolePermissions RoleOwner)
    Guest -> userId <$> createTeamMember brig galley creatorId tid (rolePermissions RoleExternalPartner)
  pure (viewerId, userA, userB, nonTeamUser)
