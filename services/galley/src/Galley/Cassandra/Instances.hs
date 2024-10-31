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
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Imports
import Wire.API.Bot ()
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Feature qualified as Public
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

instance Cql AccessRole where
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

  fromCql (CqlUdt u) =
    note "missing 'teamid' in teaminfo" ("teamid" `lookup` u) >>= fmap ConvTeamInfo . fromCql
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

  toCql = CqlInt . fromIntegral . fromEnum

  fromCql (CqlInt i) = do
    let i' = fromIntegral i
    if i' < fromEnum @ProtocolTag minBound
      || i' > fromEnum @ProtocolTag maxBound
      then Left $ "unexpected protocol: " ++ show i
      else Right $ toEnum i'
  fromCql _ = Left "protocol: int expected"

instance Cql GroupId where
  ctype = Tagged BlobColumn

  toCql = CqlBlob . LBS.fromStrict . unGroupId

  fromCql (CqlBlob b) = Right . GroupId . LBS.toStrict $ b
  fromCql _ = Left "group_id: blob expected"

instance Cql GroupInfoData where
  ctype = Tagged BlobColumn

  toCql = CqlBlob . LBS.fromStrict . unGroupInfoData
  fromCql (CqlBlob b) = Right $ GroupInfoData (LBS.toStrict b)
  fromCql _ = Left "GroupInfoData: blob expected"

instance Cql Icon where
  ctype = Tagged TextColumn
  toCql = CqlText . T.decodeUtf8 . toByteString'
  fromCql (CqlText txt) = pure . fromRight DefaultIcon . runParser parser . T.encodeUtf8 $ txt
  fromCql _ = Left "Icon: Text expected"

instance Cql Epoch where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . fromIntegral . epochNumber
  fromCql (CqlBigInt n) = pure (Epoch (fromIntegral n))
  fromCql _ = Left "epoch: bigint expected"

instance Cql ProposalRef where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unProposalRef
  fromCql (CqlBlob b) = Right . ProposalRef . LBS.toStrict $ b
  fromCql _ = Left "ProposalRef: blob expected"

instance Cql (RawMLS Proposal) where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . raw
  fromCql (CqlBlob b) = mapLeft T.unpack $ decodeMLS b
  fromCql _ = Left "Proposal: blob expected"

instance Cql CipherSuite where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . cipherSuiteNumber
  fromCql (CqlInt i) =
    if i < 2 ^ (16 :: Integer)
      then Right . CipherSuite . fromIntegral $ i
      else Left "CipherSuite: an out of bounds value for Word16"
  fromCql _ = Left "CipherSuite: int expected"

instance Cql SubConvId where
  ctype = Tagged TextColumn
  toCql = CqlText . unSubConvId
  fromCql (CqlText txt) = Right (SubConvId txt)
  fromCql _ = Left "SubConvId: Text expected"
