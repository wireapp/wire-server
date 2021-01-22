module API.TeamUserSearch (tests) where

import API.Search.Util (executeTeamUserSearch, refreshIndex)
import API.Team.Util
  ( createPopulatedBindingTeamWithNamesAndHandles,
  )
import API.User.Util (activateEmail, initiateEmailUpdateNoSend)
import Bilge (Manager, MonadHttp)
import qualified Brig.Options as Opt
import Brig.Types (SearchResult (searchResults), User (userId), fromEmail)
import Control.Monad.Catch (MonadCatch)
import Control.Retry ()
import Data.Id (TeamId, UserId)
import Imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual)
import Util
  ( Brig,
    Galley,
    randomEmail,
    test,
    withSettingsOverrides,
  )
import Wire.API.User.Search (TeamContact (teamContactCreatedAt, teamContactUserId))

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

tests :: Opt.Opts -> Manager -> Galley -> Brig -> IO TestTree
tests opts mgr _galley brig = do
  return $
    testGroup "browse team" $
      [ testWithNewIndex "by email" (testSearchByEmailSameTeam brig),
        testWithNewIndex "empty query lists the whole team sorted" (testEmptyQuerySorted brig)
      ]
  where
    testWithNewIndex name f = test mgr name $ withSettingsOverrides opts f

testSearchByEmail :: TestConstraints m => Brig -> m (TeamId, UserId, User) -> Bool -> m ()
testSearchByEmail brig mkSearcherAndSearchee canFind = do
  (tid, searcher, searchee) <- mkSearcherAndSearchee
  eml <- randomEmail
  _ <- initiateEmailUpdateNoSend brig eml (userId searchee)
  activateEmail brig eml
  refreshIndex brig
  let check = if canFind then assertTeamUserSearchCanFind else assertTeamUserSearchCannotFind
  check brig tid searcher (userId searchee) (fromEmail eml)

testSearchByEmailSameTeam :: TestConstraints m => Brig -> m ()
testSearchByEmailSameTeam brig = do
  let mkSearcherAndSearchee = do
        (tid, userId -> ownerId, [u1]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
        pure (tid, ownerId, u1)
  testSearchByEmail brig mkSearcherAndSearchee True

assertTeamUserSearchCanFind :: TestConstraints m => Brig -> TeamId -> UserId -> UserId -> Text -> m ()
assertTeamUserSearchCanFind brig teamid self expected q = do
  r <- searchResults <$> executeTeamUserSearch brig teamid self q
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q) $
      expected `elem` map teamContactUserId r

assertTeamUserSearchCannotFind :: TestConstraints m => Brig -> TeamId -> UserId -> UserId -> Text -> m ()
assertTeamUserSearchCannotFind brig teamid self expected q = do
  r <- searchResults <$> executeTeamUserSearch brig teamid self q
  liftIO $ do
    assertBool ("User shouldn't be present in results for query: " <> show q) $
      expected `notElem` map teamContactUserId r

testEmptyQuerySorted :: TestConstraints m => Brig -> m ()
testEmptyQuerySorted brig = do
  (tid, userId -> ownerId, users) <- createPopulatedBindingTeamWithNamesAndHandles brig 4
  refreshIndex brig
  r <- searchResults <$> executeTeamUserSearch brig tid ownerId ""
  let creationDates = fmap teamContactCreatedAt r
  liftIO $
    assertEqual
      "user ids"
      (sort (fmap userId users <> [ownerId]))
      (sort (fmap teamContactUserId r))
  liftIO $ assertEqual "sorted team contacts" (sortOn Down creationDates) creationDates
