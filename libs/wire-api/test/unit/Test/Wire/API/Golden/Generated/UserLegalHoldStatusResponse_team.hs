{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.UserLegalHoldStatusResponse_team where

import Data.Id (ClientId (ClientId, client))
import Data.LegalHold
  ( UserLegalHoldStatus
      ( UserLegalHoldDisabled,
        UserLegalHoldEnabled,
        UserLegalHoldPending
      ),
  )
import Imports (Maybe (Just, Nothing))
import Wire.API.Team.LegalHold (UserLegalHoldStatusResponse (..))
import Wire.API.User.Client.Prekey (lastPrekey)

testObject_UserLegalHoldStatusResponse_team_1 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_1 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\39669\&9\ENQ\1016886\11258\\3\62960x\25215")), ulhsrClientId = Just (ClientId {client = "97"})}

testObject_UserLegalHoldStatusResponse_team_2 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_2 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\111141L,")), ulhsrClientId = Just (ClientId {client = "46"})}

testObject_UserLegalHoldStatusResponse_team_3 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_3 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("W\1042917z\1923\GS")), ulhsrClientId = Just (ClientId {client = "6d"})}

testObject_UserLegalHoldStatusResponse_team_4 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_4 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_5 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_5 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("?\tvSq")), ulhsrClientId = Just (ClientId {client = "12"})}

testObject_UserLegalHoldStatusResponse_team_6 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_6 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "50"})}

testObject_UserLegalHoldStatusResponse_team_7 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_7 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Just (ClientId {client = "63"})}

testObject_UserLegalHoldStatusResponse_team_8 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_8 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_9 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_9 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "a9"})}

testObject_UserLegalHoldStatusResponse_team_10 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_10 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Just (ClientId {client = "2e"})}

testObject_UserLegalHoldStatusResponse_team_11 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_11 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_12 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_12 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_13 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_13 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("=~\CAN\15127jSe\STX")), ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_14 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_14 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("jO\167324\rT\1028195")), ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_15 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_15 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("\DLE{\STX")), ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_16 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_16 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("}\65064LE\179801E")), ulhsrClientId = Nothing}

testObject_UserLegalHoldStatusResponse_team_17 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_17 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldDisabled, ulhsrLastPrekey = Just (lastPrekey ("\NAK \GS\1080662\&9,'<\a\8244")), ulhsrClientId = Just (ClientId {client = "7a"})}

testObject_UserLegalHoldStatusResponse_team_18 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_18 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Just (lastPrekey ("Z")), ulhsrClientId = Just (ClientId {client = "ba"})}

testObject_UserLegalHoldStatusResponse_team_19 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_19 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldEnabled, ulhsrLastPrekey = Just (lastPrekey ("")), ulhsrClientId = Just (ClientId {client = "88"})}

testObject_UserLegalHoldStatusResponse_team_20 :: UserLegalHoldStatusResponse
testObject_UserLegalHoldStatusResponse_team_20 = UserLegalHoldStatusResponse {ulhsrStatus = UserLegalHoldPending, ulhsrLastPrekey = Nothing, ulhsrClientId = Nothing}
