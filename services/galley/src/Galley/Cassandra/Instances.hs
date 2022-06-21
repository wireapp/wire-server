{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Galley.Cassandra.Instances
  (
  )
where

import Cassandra.CQL
import Control.Error (note)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain, domainText, mkDomain)
import qualified Data.Text.Encoding as T
import Galley.Types.Bot ()
import Galley.Types.Teams.Intra
import Imports
import Wire.API.Asset (AssetKey, assetKeyToText)
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Team
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.SearchVisibility

deriving instance Cql MutedStatus

deriving instance Cql ReceiptMode

instance Cql ConvType where
  ctype = Tagged IntColumn

  toCql RegularConv = CqlInt 0
  toCql SelfConv = CqlInt 1
  toCql One2OneConv = CqlInt 2
  toCql ConnectConv = CqlInt 3

  fromCql (CqlInt i) = case i of
    0 -> pure RegularConv
    1 -> pure SelfConv
    2 -> pure One2OneConv
    3 -> pure ConnectConv
    n -> Left $ "unexpected conversation-type: " ++ show n
  fromCql _ = Left "conv-type: int expected"

instance Cql Access where
  ctype = Tagged IntColumn

  toCql PrivateAccess = CqlInt 1
  toCql InviteAccess = CqlInt 2
  toCql LinkAccess = CqlInt 3
  toCql CodeAccess = CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> pure PrivateAccess
    2 -> pure InviteAccess
    3 -> pure LinkAccess
    4 -> pure CodeAccess
    n -> Left $ "Unexpected Access value: " ++ show n
  fromCql _ = Left "Access value: int expected"

instance Cql AccessRoleLegacy where
  ctype = Tagged IntColumn

  toCql PrivateAccessRole = CqlInt 1
  toCql TeamAccessRole = CqlInt 2
  toCql ActivatedAccessRole = CqlInt 3
  toCql NonActivatedAccessRole = CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> pure PrivateAccessRole
    2 -> pure TeamAccessRole
    3 -> pure ActivatedAccessRole
    4 -> pure NonActivatedAccessRole
    n -> Left $ "Unexpected AccessRole value: " ++ show n
  fromCql _ = Left "AccessRole value: int expected"

instance Cql AccessRoleV2 where
  ctype = Tagged IntColumn

  toCql = \case
    TeamMemberAccessRole -> CqlInt 1
    NonTeamMemberAccessRole -> CqlInt 2
    GuestAccessRole -> CqlInt 3
    ServiceAccessRole -> CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> pure TeamMemberAccessRole
    2 -> pure NonTeamMemberAccessRole
    3 -> pure GuestAccessRole
    4 -> pure ServiceAccessRole
    n -> Left $ "Unexpected AccessRoleV2 value: " ++ show n
  fromCql _ = Left "AccessRoleV2 value: int expected"

instance Cql ConvTeamInfo where
  ctype = Tagged $ UdtColumn "teaminfo" [("teamid", UuidColumn), ("managed", BooleanColumn)]

  toCql t = CqlUdt [("teamid", toCql (cnvTeamId t)), ("managed", toCql False)]

  fromCql (CqlUdt u) = do
    t <- note "missing 'teamid' in teaminfo" ("teamid" `lookup` u) >>= fromCql
    pure (ConvTeamInfo t)
  fromCql _ = Left "teaminfo: udt expected"

instance Cql TeamBinding where
  ctype = Tagged BooleanColumn

  toCql Binding = CqlBoolean True
  toCql NonBinding = CqlBoolean False

  fromCql (CqlBoolean True) = pure Binding
  fromCql (CqlBoolean False) = pure NonBinding
  fromCql _ = Left "teambinding: boolean expected"

instance Cql TeamStatus where
  ctype = Tagged IntColumn

  toCql Active = CqlInt 0
  toCql PendingDelete = CqlInt 1
  toCql Deleted = CqlInt 2
  toCql Suspended = CqlInt 3
  toCql PendingActive = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> pure Active
    1 -> pure PendingDelete
    2 -> pure Deleted
    3 -> pure Suspended
    4 -> pure PendingActive
    n -> Left $ "unexpected team-status: " ++ show n
  fromCql _ = Left "team-status: int expected"

instance Cql TeamSearchVisibility where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ SearchVisibilityStandard
    1 -> pure $ SearchVisibilityNoNameOutsideTeam
    _ -> Left "fromCql: Invalid TeamSearchVisibility"
  fromCql _ = Left "fromCql: TeamSearchVisibility: CqlInt expected"

  toCql SearchVisibilityStandard = CqlInt 0
  toCql SearchVisibilityNoNameOutsideTeam = CqlInt 1

instance Cql Domain where
  ctype = Tagged TextColumn
  toCql = CqlText . domainText
  fromCql (CqlText txt) = mkDomain txt
  fromCql _ = Left "Domain: Text expected"

instance Cql Public.EnforceAppLock where
  ctype = Tagged IntColumn
  toCql (Public.EnforceAppLock False) = CqlInt 0
  toCql (Public.EnforceAppLock True) = CqlInt 1
  fromCql (CqlInt n) = case n of
    0 -> pure (Public.EnforceAppLock False)
    1 -> pure (Public.EnforceAppLock True)
    _ -> Left "fromCql EnforceAppLock: int out of range"
  fromCql _ = Left "fromCql EnforceAppLock: int expected"

instance Cql ProtocolTag where
  ctype = Tagged IntColumn

  toCql ProtocolProteusTag = CqlInt 0
  toCql ProtocolMLSTag = CqlInt 1

  fromCql (CqlInt i) = case i of
    0 -> pure ProtocolProteusTag
    1 -> pure ProtocolMLSTag
    n -> Left $ "unexpected protocol: " ++ show n
  fromCql _ = Left "protocol: int expected"

instance Cql GroupId where
  ctype = Tagged BlobColumn

  toCql = CqlBlob . LBS.fromStrict . unGroupId

  fromCql (CqlBlob b) = Right . GroupId . LBS.toStrict $ b
  fromCql _ = Left "group_id: blob expected"

instance Cql Icon where
  ctype = Tagged TextColumn
  toCql = CqlText . T.decodeUtf8 . toByteString'
  fromCql (CqlText txt) = pure . fromRight DefaultIcon . runParser parser . T.encodeUtf8 $ txt
  fromCql _ = Left "Icon: Text expected"

instance Cql AssetKey where
  ctype = Tagged TextColumn
  toCql = CqlText . assetKeyToText
  fromCql (CqlText txt) = runParser parser . T.encodeUtf8 $ txt
  fromCql _ = Left "AssetKey: Text expected"

instance Cql Epoch where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . fromIntegral . epochNumber
  fromCql (CqlBigInt n) = pure (Epoch (fromIntegral n))
  fromCql _ = Left "epoch: bigint expected"
