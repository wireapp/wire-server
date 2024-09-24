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

module API.User.RichInfo
  ( tests,
  )
where

import API.RichInfo.Util
import API.Team.Util (createTeamMember, createUserWithTeam)
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Options
import Brig.Options qualified as Opt
import Data.CaseInsensitive qualified as CI
import Data.List1 qualified as List1
import Data.Text qualified as Text
import Imports
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util
import Util.Timeout
import Wire.API.Team.Permission
import Wire.API.User
import Wire.API.User.RichInfo

tests :: ConnectionLimit -> Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at conf p b _c g =
  testGroup
    "rich info"
    [ test p "there is default empty rich info" $ testDefaultRichInfo b g,
      test p "missing fields in an update are deleted" $ testDeleteMissingFieldsInUpdates b g,
      test p "fields with empty strings are deleted" $ testDeleteEmptyFields b g,
      test p "duplicate field names are silently nubbed (first entry wins)" $ testDedupeDuplicateFieldNames b,
      test p "exceeding rich info size limit is forbidden" $ testRichInfoSizeLimit b conf,
      test p "non-team members don't have rich info" $ testNonTeamMembersDoNotHaveRichInfo b,
      test p "non-members / other membes / guests cannot see rich info" $ testGuestsCannotSeeRichInfo b
    ]

-- | Test that for team members there is rich info set by default, and that it's empty.
testDefaultRichInfo :: Brig -> Galley -> Http ()
testDefaultRichInfo brig galley = do
  -- Create a team with two users
  (owner, tid) <- createUserWithTeam brig
  member1 <- userId <$> createTeamMember brig galley owner tid noPermissions
  member2 <- userId <$> createTeamMember brig galley owner tid noPermissions
  -- The first user should see the second user's rich info and it should be empty
  richInfo <- getRichInfo brig member1 member2
  liftIO $
    assertEqual
      "rich info is not empty, or not present"
      (Right (mkRichInfoAssocList mempty))
      richInfo

testDeleteMissingFieldsInUpdates :: Brig -> Galley -> Http ()
testDeleteMissingFieldsInUpdates brig galley = do
  (owner, tid) <- createUserWithTeam brig
  member1 <- userId <$> createTeamMember brig galley owner tid noPermissions
  member2 <- userId <$> createTeamMember brig galley owner tid noPermissions
  let superset =
        mkRichInfoAssocList
          [ RichField "department" "blue",
            RichField "relevance" "meh"
          ]
      subset =
        mkRichInfoAssocList
          [ RichField "relevance" "meh"
          ]
  putRichInfo brig member2 superset !!! const 200 === statusCode
  putRichInfo brig member2 subset !!! const 200 === statusCode
  subset' <- getRichInfo brig member1 member2
  liftIO $ assertEqual "dangling rich info fields" (Right subset) subset'

testDeleteEmptyFields :: Brig -> Galley -> Http ()
testDeleteEmptyFields brig galley = do
  (owner, tid) <- createUserWithTeam brig
  member1 <- userId <$> createTeamMember brig galley owner tid noPermissions
  member2 <- userId <$> createTeamMember brig galley owner tid noPermissions
  let withEmpty =
        mkRichInfoAssocList
          [ RichField "department" ""
          ]
  putRichInfo brig member2 withEmpty !!! const 200 === statusCode
  withoutEmpty <- getRichInfo brig member1 member2
  liftIO $ assertEqual "dangling rich info fields" (Right mempty) withoutEmpty

testDedupeDuplicateFieldNames :: Brig -> Http ()
testDedupeDuplicateFieldNames brig = do
  (owner, _) <- createUserWithTeam brig
  let dupes =
        mkRichInfoAssocList
          [ RichField "dePartment" "blue",
            RichField "Department" "green"
          ]
      deduped =
        mkRichInfoAssocList
          [ RichField "departMent" "blue"
          ]
  putRichInfo brig owner dupes !!! const 200 === statusCode
  ri <- getRichInfo brig owner owner
  liftIO $ assertEqual "duplicate rich info fields" (Right deduped) ri

testRichInfoSizeLimit :: (HasCallStack) => Brig -> Opt.Opts -> Http ()
testRichInfoSizeLimit brig conf = do
  let maxSize :: Int = conf.settings.richInfoLimit
  (owner, _) <- createUserWithTeam brig
  let bad1 =
        mkRichInfoAssocList
          [ RichField "department" (Text.replicate (fromIntegral maxSize) "#")
          ]
      bad2 =
        mkRichInfoAssocList $
          [0 .. (maxSize `div` 2)]
            <&> \i -> RichField (CI.mk $ Text.pack $ show i) "#"
  putRichInfo brig owner bad1 !!! const 413 === statusCode
  putRichInfo brig owner bad2 !!! const 413 === statusCode

-- | Test that rich info of non-team members can not be queried.
--
-- Note: it would be nice to also test that non-team members can not set rich info, but for
-- now there's no public endpoint for setting it in Brig, so we can't do anything.
testNonTeamMembersDoNotHaveRichInfo :: Brig -> Http ()
testNonTeamMembersDoNotHaveRichInfo brig = do
  -- Create a user
  targetUser <- userId <$> randomUser brig
  -- Another user should get a 'Nothing' when querying their info
  do
    nonTeamUser <- userId <$> randomUser brig
    richInfo <- getRichInfo brig nonTeamUser targetUser
    liftIO $ assertEqual "rich info is present" (Left 403) richInfo
  -- A team member should also get a 'Nothing' when querying their info
  do
    (teamUser, _) <- createUserWithTeam brig
    richInfo <- getRichInfo brig teamUser targetUser
    liftIO $ assertEqual "rich info is present" (Left 403) richInfo

testGuestsCannotSeeRichInfo :: Brig -> Http ()
testGuestsCannotSeeRichInfo brig = do
  -- Create a team
  (owner, _) <- createUserWithTeam brig
  -- A non-team user should get "forbidden" when querying rich info for team user.
  do
    nonTeamUser <- userId <$> randomUser brig
    richInfo <- getRichInfo brig nonTeamUser owner
    liftIO $ assertEqual "rich info status /= 403" (Left 403) richInfo
  -- ...  even if she's a guest in the team...
  do
    guest <- userId <$> randomUser brig
    connectUsers brig owner (List1.singleton guest)
    richInfo <- getRichInfo brig guest owner
    liftIO $ assertEqual "rich info status /= 403" (Left 403) richInfo
  -- ...  or if she's in another team.
  do
    (otherOwner, _) <- createUserWithTeam brig
    richInfo <- getRichInfo brig otherOwner owner
    liftIO $ assertEqual "rich info status /= 403" (Left 403) richInfo
