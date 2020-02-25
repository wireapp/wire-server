module API.Search (tests) where

import API.Search.Util
import Bilge
import Brig.Types
import Data.Handle (fromHandle)
import Imports
import Network.HTTP.Client (Manager)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Concurrently (..), runConcurrently)
import Util

tests :: Manager -> Brig -> IO TestTree
tests mgr brig =
  return $
    testGroup
      "search"
      [ test mgr "opt-in-out" $ testOptInOut brig,
        test mgr "by-name" $ testSearchByName brig,
        test mgr "by-handle" $ testSearchByHandle brig,
        test mgr "reindex" $ testReindex brig
      ]

testOptInOut :: Brig -> Http ()
testOptInOut brig = do
  u1 <- randomUserWithHandle brig
  u2 <- randomUser brig
  refreshIndex brig
  let uid1 = userId u1
      uid2 = userId u2
      Just h1 = fromHandle <$> userHandle u1
  assertSearchable "default" brig uid1 True
  assertCanFind brig uid2 uid1 h1
  updateSearchableStatus brig uid1 optOut
  refreshIndex brig
  assertSearchable "opted out" brig uid1 False
  assertCan'tFind brig uid2 uid1 h1
  updateSearchableStatus brig uid1 optIn
  refreshIndex brig
  assertSearchable "opted in" brig uid1 True
  assertCanFind brig uid2 uid1 h1

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
  ((), regular, tinfoil, unfoil) <-
    runConcurrently $
      (,,,)
        <$> Concurrently (reindex brig)
        <*> Concurrently (replicateM 5 $ delayed *> mkRegularUser)
        <*> Concurrently (replicateM 5 $ delayed *> mkInvisibleUser)
        <*> Concurrently (replicateM 5 $ delayed *> mkTmpInvisibleUser)
  refreshIndex brig
  for_ tinfoil $ \u' ->
    let Just h = fromHandle <$> userHandle u'
     in assertCan'tFind brig (userId u) (userId u') h
  for_ (regular <> unfoil) $ \u' -> do
    let Just h = fromHandle <$> userHandle u'
    assertCanFind brig (userId u) (userId u') h
    Just (found : _) <- fmap searchResults <$> executeSearch brig (userId u) h
    liftIO $ do
      assertEqual "Unexpected UserId" (contactUserId found) (userId u')
      assertEqual "Unexpected Name" (contactName found) (fromName $ userName u')
      assertEqual "Unexpected Colour" (contactColorId found) (Just . fromIntegral . fromColourId $ userAccentId u')
      assertEqual "Unexpected Handle" (contactHandle found) (fromHandle <$> userHandle u')
  where
    -- note: delaying user creation a bit to increase the chance of actually
    -- happen concurrently to the reindex on a small test database
    delayed = liftIO $ threadDelay 10000
    mkRegularUser = randomUserWithHandle brig
    mkInvisibleUser = do
      u <- randomUserWithHandle brig
      updateSearchableStatus brig (userId u) optOut
      pure u
    mkTmpInvisibleUser = do
      u <- randomUserWithHandle brig
      updateSearchableStatus brig (userId u) optOut
      updateSearchableStatus brig (userId u) optIn
      pure u
