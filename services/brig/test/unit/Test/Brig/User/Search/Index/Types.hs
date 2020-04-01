{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Brig.User.Search.Index.Types where

import Brig.Types.Common
import Brig.User.Search.Index
import Data.Aeson
import Data.Handle
import Data.Id
import Data.UUID
import Database.Bloodhound.Internal.Client
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "UserDoc, IndexUser: conversion, serialization"
    [ testCase "aeson roundtrip: UserDoc" $
        assertEqual
          "failed"
          (eitherDecode' (encode userDoc1))
          (Right userDoc1),
      testCase "aeson half-roundtrip: UserDoc" $
        assertEqual
          "failed"
          (encode userDoc1)
          userDoc1ByteString,
      testCase "aeson roundtrip: UserDoc" $
        assertEqual
          "failed"
          (fromJust . decode @Value . encode $ userDoc1)
          (fromJust . decode @Value $ userDoc1ByteString),
      testCase "IndexUser to UserDoc" $
        assertEqual
          "failed"
          (userDoc indexUser1)
          userDoc1
    ]

userDoc1 :: UserDoc
userDoc1 =
  UserDoc
    { udId = Id . fromJust . fromText $ "0a96b396-57d6-11ea-a04b-7b93d1a5c19c",
      udTeam = Just . Id . fromJust . fromText $ "17c59b18-57d6-11ea-9220-8bbf5eee961a",
      udName = Just . Name $ "Carl Phoomp",
      udNormalized = Just $ "carl phoomp",
      udHandle = Just . fromJust . parseHandle $ "phoompy",
      udColourId = Just . ColourId $ 32
    }

userDoc1ByteString :: LByteString
userDoc1ByteString = "{\"team\":\"17c59b18-57d6-11ea-9220-8bbf5eee961a\",\"handle\":\"phoompy\",\"accent_id\":32,\"name\":\"Carl Phoomp\",\"id\":\"0a96b396-57d6-11ea-a04b-7b93d1a5c19c\",\"normalized\":\"carl phoomp\"}"

indexUser1 :: IndexUser
indexUser1 =
  IndexUser
    { _iuUserId = udId userDoc1,
      _iuVersion = IndexVersion (DocVersion 1),
      _iuTeam = udTeam userDoc1,
      _iuName = udName userDoc1,
      _iuHandle = udHandle userDoc1,
      _iuColourId = udColourId userDoc1
    }
