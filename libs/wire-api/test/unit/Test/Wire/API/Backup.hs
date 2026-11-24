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
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Wire.API.Backup (tests) where

import Data.Aeson (object, toJSON, (.=))
import Data.Id (ClientId)
import Data.Json.Util (toUTCTimeMillis)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.UUID qualified as UUID
import Imports
import Servant.API (parseUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Positive (..), testProperty, (===))
import Wire.API.Backup

tests :: TestTree
tests =
  testGroup
    "Backup"
    [ testCase "BackupEpoch rejects 0" $ do
        assertBool "0 must be rejected" (isLeft (parseUrlPiece @BackupEpoch "0"))
        parseUrlPiece @BackupEpoch "1" @?= Right (BackupEpoch 1)
        parseUrlPiece @BackupEpoch "4294967295" @?= Right (BackupEpoch maxBound)
        assertBool "2^32 must be rejected" (isLeft (parseUrlPiece @BackupEpoch "4294967296"))
        minBound @?= BackupEpoch 1,
      testCase "epochAccepted without epochs only accepts 1" $ do
        assertBool "1" (epochAccepted Nothing (BackupEpoch 1))
        assertBool "2" (not (epochAccepted Nothing (BackupEpoch 2))),
      testProperty "epochAccepted accepts max and max + 1 only" $ \(Positive (n :: Word32)) ->
        let m = BackupEpoch (n `mod` 1_000_000 + 2)
            BackupEpoch k = m
         in map (epochAccepted (Just m) . BackupEpoch) [k - 1, k, k + 1, k + 2]
              === [False, True, True, False],
      testCase "epochAcceptedNoOverflow" $ do
        assertBool
          "maxBound + 1 must not wrap to 1"
          (not (epochAccepted (Just maxBound) (BackupEpoch 1)))
        assertBool "maxBound itself is accepted" (epochAccepted (Just maxBound) maxBound),
      testCase "staleEpochs is e - 2" $
        staleEpochs (BackupEpoch 5) @?= BackupEpoch 3,
      testCase "BackupFile JSON matches RFC example" $ do
        let fid = BackupFileId (fromJust (UUID.fromString "99db9768-04e3-4b5d-9268-831b6a25c4ab"))
            t = UTCTime (fromGregorian 2026 11 12) (secondsToDiffTime (10 * 3600 + 15 * 60 + 30) + 0.123)
            file = BackupFile {id = fid, size = 1048576, createdAt = toUTCTimeMillis t}
        toJSON file
          @?= object
            [ "id" .= ("99db9768-04e3-4b5d-9268-831b6a25c4ab" :: Text),
              "size" .= (1048576 :: Int),
              "createdAt" .= ("2026-11-12T10:15:30.123Z" :: Text)
            ],
      testCase "BackupCleanupResult JSON" $
        toJSON (BackupCleanupResult 3) @?= object ["deleted" .= (3 :: Int)],
      testCase "BackupList JSON matches RFC example" $ do
        let t = UTCTime (fromGregorian 2026 11 12) (secondsToDiffTime (10 * 3600 + 35 * 60 + 30) + 0.456)
            summary =
              EpochSummary
                { epoch = BackupEpoch 2,
                  createdAt = toUTCTimeMillis t,
                  fileCount = 2,
                  size = 1050624
                }
            cid = fromRight (error "bad client id") (parseUrlPiece @ClientId "4b2f7a9c1d3e5f60")
        toJSON (BackupList [ClientBackups {client = cid, epochs = [summary]}])
          @?= object
            [ "clients"
                .= [ object
                       [ "client" .= ("4b2f7a9c1d3e5f60" :: Text),
                         "epochs"
                           .= [ object
                                  [ "epoch" .= (2 :: Int),
                                    "createdAt" .= ("2026-11-12T10:35:30.456Z" :: Text),
                                    "fileCount" .= (2 :: Int),
                                    "size" .= (1050624 :: Int)
                                  ]
                              ]
                       ]
                   ]
            ]
    ]
