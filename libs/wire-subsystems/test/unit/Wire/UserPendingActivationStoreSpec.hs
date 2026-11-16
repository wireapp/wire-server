-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserPendingActivationStoreSpec (spec) where

import Data.Id
import Data.Map qualified as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Imports
import Polysemy
import Polysemy.State
import Test.Hspec
import Test.QuickCheck
import Wire.MockInterpreters.UserPendingActivationStore
import Wire.UserPendingActivationStore

runStore ::
  [UserPendingActivation] ->
  Sem '[UserPendingActivationStore, State (Map UserId UTCTime)] a ->
  (Map UserId UTCTime, a)
runStore initial =
  run
    . runState (Map.fromList [(upa.upaUserId, upa.upaDay) | upa <- initial])
    . inMemoryUserPendingActivationStoreInterpreter

mkEntries :: Int -> IO [UserPendingActivation]
mkEntries n =
  for [1 .. n] $ \i -> do
    uid <- generate arbitrary
    pure $ UserPendingActivation uid (posixSecondsToUTCTime (fromIntegral i))

-- | Collect all pages, optionally removing every visited entry like the
-- expiry cleanup does.
collectPages :: Bool -> Int32 -> Sem '[UserPendingActivationStore, State (Map UserId UTCTime)] [[UserPendingActivation]]
collectPages removeVisited limit = execState [] $ forEachPage (`listPage` limit) $ \items -> do
  modify (<> [items])
  when removeVisited $ removeMultiple (map (.upaUserId) items)

spec :: Spec
spec = describe "UserPendingActivationStore" $ do
  describe "ListPage" $ do
    it "returns an empty last page for an empty store" $ do
      snd (runStore [] (listPage Nothing 3)) `shouldBe` ([], False)

    for_ [(2, False), (3, False), (4, True)] $ \(n, more) ->
      it ("reports more entries correctly for " <> show n <> " entries and a page size of 3") $ do
        entries <- mkEntries n
        let (items, hasMore) = snd (runStore entries (listPage Nothing 3))
        hasMore `shouldBe` more
        items `shouldBe` take 3 (sortOn (.upaUserId) entries)

    it "continues after an id that has been removed" $ do
      entries <- sortOn (.upaUserId) <$> mkEntries 5
      let pivot = (entries !! 1).upaUserId
          (_, (items, hasMore)) = runStore entries $ do
            remove pivot
            listPage (Just pivot) 10
      items `shouldBe` drop 2 entries
      hasMore `shouldBe` False

  describe "forEachPage" $ do
    for_ [0, 1, 2, 3, 4, 6, 7] $ \n ->
      for_ [False, True] $ \removeVisited ->
        it ("visits each of " <> show n <> " entries exactly once (page size 3, removing visited: " <> show removeVisited <> ")") $ do
          entries <- mkEntries n
          let (final, pages) = runStore entries (collectPages removeVisited 3)
          sortOn (.upaUserId) (concat pages) `shouldBe` sortOn (.upaUserId) entries
          all ((<= 3) . length) pages `shouldBe` True
          Map.size final `shouldBe` (if removeVisited then 0 else n)

  describe "RemoveMultiple" $ do
    it "removes exactly the given entries and ignores unknown ids" $ do
      entries <- mkEntries 4
      unknown <- generate arbitrary
      let (toRemove, toKeep) = splitAt 2 entries
          (final, ()) = runStore entries (removeMultiple (unknown : map (.upaUserId) toRemove))
      Map.keys final `shouldBe` sort (map (.upaUserId) toKeep)
