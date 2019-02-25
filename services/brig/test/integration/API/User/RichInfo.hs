module API.User.RichInfo (tests) where

import Imports
import API.Team.Util (createUserWithTeam, createTeamMember)
import API.User.Util
import API.RichInfo.Util
import Bilge hiding (accept, timeout)
import Brig.Types
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Util

import qualified Brig.Options                as Opt
import qualified Galley.Types.Teams          as Team

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at _conf p b _c g = testGroup "rich info"
    [ test p "there is default empty rich info" $ testDefaultRichInfo b g
    , test p "non-team members don't have rich info" $ testNonTeamMembersDoNotHaveRichInfo b g
    ]

-- TODO Guests can't see rich info
-- TODO Members of other teams can't see rich info
-- TODO Empty fields are deleted

-- | Test that for team members there is rich info set by default, and that it's empty.
testDefaultRichInfo :: Brig -> Galley -> Http ()
testDefaultRichInfo brig galley = do
    -- Create a team with two users
    (owner, tid) <- createUserWithTeam brig galley
    member1 <- userId <$> createTeamMember brig galley owner tid Team.noPermissions
    member2 <- userId <$> createTeamMember brig galley owner tid Team.noPermissions
    -- The first user should see the second user's rich info and it should be empty
    richInfo <- getRichInfo brig member1 member2
    liftIO $ assertEqual "rich info is not empty, or not present"
        richInfo
        (Just (RichInfo {richInfoFields = []}))

-- | Test that rich info of non-team members can not be queried.
--
-- Note: it would be nice to also test that non-team members can not set rich info, but for
-- now there's no public endpoint for setting it in Brig, so we can't do anything.
testNonTeamMembersDoNotHaveRichInfo :: Brig -> Galley -> Http ()
testNonTeamMembersDoNotHaveRichInfo brig galley = do
    -- Create a user
    targetUser <- userId <$> randomUser brig
    -- Another user should get a 'Nothing' when querying their info
    do nonTeamUser <- userId <$> randomUser brig
       richInfo <- getRichInfo brig nonTeamUser targetUser
       liftIO $ assertEqual "rich info is present" richInfo Nothing
    -- A team member should also get a 'Nothing' when querying their info
    do (teamUser, _) <- createUserWithTeam brig galley
       richInfo <- getRichInfo brig teamUser targetUser
       liftIO $ assertEqual "rich info is present" richInfo Nothing
