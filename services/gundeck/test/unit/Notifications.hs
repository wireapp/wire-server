-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Notifications where

import Cassandra
import Data.ByteString.Lazy qualified as L
import Data.UUID.V1
import Gundeck.Notification.Data
import Imports
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "notifications"
    [ testCase "collect empty page" testCollectEmpty,
      testCase "truncate page with large payloads" testCollectTruncated
    ]

newtype TestCollectM a = TestCollectM
  {runTestCollectM :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

largePayload :: LByteString
largePayload = mconcat (replicate 100 "developers ")

largePayloadSize :: Int32
largePayloadSize = fromIntegral $ L.length largePayload

instance FetchPayloads TestCollectM where
  fetchPayload c (id_, mpl, _, _, mcs) = do
    let pl = fromMaybe (Blob largePayload) mpl
    pure $ toNotifSingle c (id_, pl, mcs)
  fetchPayloads c rows = catMaybes <$> traverse (fetchPayload c) rows

emptyTestPage :: TestCollectM (NotifPage TestCollectM)
emptyTestPage = pure $ NotifPage [] False emptyTestPage

testCollectEmpty :: IO ()
testCollectEmpty = do
  r <- runTestCollectM $ collect Nothing mempty True 11 1000 emptyTestPage
  r @?= (mempty, False)

testCollectTruncated :: IO ()
testCollectTruncated = do
  ns <- replicateM 10 $ do
    nId <- TimeUuid . fromJust <$> nextUUID
    pure (nId, Nothing, Nothing, Just largePayloadSize, Nothing)
  let getPage = pure $ NotifPage ns False emptyTestPage
  (rs, more) <- runTestCollectM $ collect Nothing mempty True 5 500 getPage
  more @?= True
  length rs @?= 0
