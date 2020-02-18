module API.Search (tests) where

import API.Search.Util
import API.Team.Util (createPopulatedBindingTeam)
import Bilge
import Brig.Types
import Imports
import Network.HTTP.Client (Manager)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Concurrently (..), runConcurrently)
import Util

tests :: Manager -> Galley -> Brig -> IO TestTree
tests mgr galley brig =
    return $ testGroup "search"
        [ test mgr "by-name"    $ testSearchByName brig
        , test mgr "by-handle"  $ testSearchByHandle brig
        , test mgr "reindex"    $ testReindex brig

        , test mgr "team member cannot be found by non-team user"        $ testSearchTeamMemberAsNonMember galley brig
        , test mgr "team A member cannot be found by team B member"      $ testSearchTeamMemberAsOtherMember galley brig
        , test mgr "team A member *can* be found by other team A member" $ testSearchTeamMemberAsSameMember galley brig
        , test mgr "non team user can be found by a team member"         $ testSeachNonMemberAsTeamMember galley brig
        ]

testSearchByName :: Brig -> Http ()
testSearchByName brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
  assertCanFind brig uid1 uid2 (fromName (userName u2))
  assertCanFind brig uid2 uid1 (fromName (userName u1))
  -- Users cannot find themselves
  assertCan'tFind brig uid1 uid1 (fromName (userName u1))
  assertCan'tFind brig uid2 uid2 (fromName (userName u2))

testSearchByHandle :: Brig -> Http ()
testSearchByHandle brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
      Just h = fromHandle <$> userHandle u1
  assertCanFind brig uid2 uid1 h

testReindex :: Brig -> Http ()
testReindex brig = do
    u <- randomUser brig

    ((), regular) <- runConcurrently $ (,)
        <$> Concurrently (reindex brig)
        <*> Concurrently (replicateM 5 $ delayed *> mkRegularUser)

    refreshIndex brig

    for_ regular $ \u' -> do
        let Just h = fromHandle <$> userHandle u'
        assertCanFind brig (userId u) (userId u') h
        Just (found:_) <- fmap searchResults <$> executeSearch brig (userId u) h
        liftIO $ do
            assertEqual "Unexpected UserId" (contactUserId  found) (userId u')
            assertEqual "Unexpected Name"   (contactName    found) (fromName $ userName u')
            assertEqual "Unexpected Colour" (contactColorId found) (Just . fromIntegral . fromColourId  $ userAccentId u')
            assertEqual "Unexpected Handle" (contactHandle  found) (fromHandle <$> userHandle u')
  where
    -- note: delaying user creation a bit to increase the chance of actually
    -- happen concurrently to the reindex on a small test database
    delayed = liftIO $ threadDelay 10000
    mkRegularUser = randomUserWithHandle brig

testSearchTeamMemberAsNonMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsNonMember galley brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig galley 1
  refreshIndex brig
  assertCan'tFind brig (userId nonTeamMember) (userId teamMember) (fromName (userName teamMember))

testSearchTeamMemberAsOtherMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsOtherMember galley brig = do
  (_, _, [teamAMember]) <- createPopulatedBindingTeam brig galley 1
  (_, _, [teamBMember]) <- createPopulatedBindingTeam brig galley 1
  refreshIndex brig
  assertCan'tFind brig (userId teamAMember) (userId teamBMember) (fromName (userName teamBMember))

testSearchTeamMemberAsSameMember :: Galley -> Brig -> Http ()
testSearchTeamMemberAsSameMember galley brig = do
  (_, _, [teamAMember, teamAMember']) <- createPopulatedBindingTeam brig galley 2
  refreshIndex brig
  assertCanFind brig (userId teamAMember) (userId teamAMember') (fromName (userName teamAMember'))

testSeachNonMemberAsTeamMember :: Galley -> Brig -> Http ()
testSeachNonMemberAsTeamMember galley brig = do
  nonTeamMember <- randomUser brig
  (_, _, [teamMember]) <- createPopulatedBindingTeam brig galley 1
  refreshIndex brig
  assertCanFind brig (userId teamMember) (userId nonTeamMember) (fromName (userName nonTeamMember))
