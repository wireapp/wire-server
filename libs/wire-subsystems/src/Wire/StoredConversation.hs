{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.StoredConversation where

import Data.Default
import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Data.UUID.Tagged qualified as U
import Galley.Types.Teams (isTeamMember)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group.Serialisation qualified as MLS
import Wire.API.MLS.SubConversation
import Wire.API.Provider.Service
import Wire.API.Team.Member
import Wire.API.User
import Wire.UserList

-- | Internal conversation type, corresponding directly to database schema.
-- Should never be sent to users (and therefore doesn't have 'FromJSON' or
-- 'ToJSON' instances).
data StoredConversation = StoredConversation
  { id_ :: ConvId,
    localMembers :: [LocalMember],
    remoteMembers :: [RemoteMember],
    metadata :: ConversationMetadata,
    protocol :: Protocol
  }
  deriving (Show)

instance HasCellsState StoredConversation where
  getCellsState = getCellsState . (.metadata)

type ConvRowWithId =
  ( ConvId,
    ConvType,
    Maybe UserId,
    Maybe [Access],
    Maybe (Set AccessRole),
    Maybe Text,
    Maybe TeamId,
    Maybe Milliseconds,
    Maybe ReceiptMode,
    Maybe ProtocolTag,
    Maybe GroupId,
    Maybe Epoch,
    Maybe UTCTime,
    Maybe CipherSuiteTag,
    Maybe GroupConvType,
    Maybe AddPermission,
    Maybe CellsState,
    Maybe ConvId
  )

type ConvRow =
  ( ConvType,
    Maybe UserId,
    Maybe [Access],
    Maybe (Set AccessRole),
    Maybe Text,
    Maybe TeamId,
    Maybe Milliseconds,
    Maybe ReceiptMode,
    Maybe ProtocolTag,
    Maybe GroupId,
    Maybe Epoch,
    Maybe UTCTime,
    Maybe CipherSuiteTag,
    Maybe GroupConvType,
    Maybe AddPermission,
    Maybe CellsState,
    Maybe ConvId
  )

splitIdFromRow :: ConvRowWithId -> (ConvId, ConvRow)
splitIdFromRow (convId, cty, muid, acc, roleV2, nme, ti, timer, rm, ptag, mgid, mep, mts, mcs, mgct, mAp, mcells, mparent) =
  (convId, (cty, muid, acc, roleV2, nme, ti, timer, rm, ptag, mgid, mep, mts, mcs, mgct, mAp, mcells, mparent))

toProtocol ::
  Maybe ProtocolTag ->
  Maybe GroupId ->
  Maybe Epoch ->
  Maybe UTCTime ->
  Maybe CipherSuiteTag ->
  Maybe Protocol
toProtocol Nothing _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolProteusTag) _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolMLSTag) mgid mepoch mtimestamp mcs = ProtocolMLS <$> toConversationMLSData mgid mepoch mtimestamp mcs
toProtocol (Just ProtocolMixedTag) mgid mepoch mtimestamp mcs = ProtocolMixed <$> toConversationMLSData mgid mepoch mtimestamp mcs

toConversationMLSData :: Maybe GroupId -> Maybe Epoch -> Maybe UTCTime -> Maybe CipherSuiteTag -> Maybe ConversationMLSData
toConversationMLSData mgid mepoch mtimestamp mcs =
  ConversationMLSData
    <$> mgid
    <*> pure
      ( ActiveMLSConversationData
          <$> mepoch
          <*> mtimestamp
          <*> mcs
      )

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe ConvRow ->
  Maybe StoredConversation
toConv cid ms remoteMems mconv = do
  row@(_, _, _, _, _, _, _, _, ptag, mgid, mep, mts, mcs, _, _, _, _) <- mconv
  proto <- toProtocol ptag mgid mep mts mcs
  pure
    StoredConversation
      { id_ = cid,
        localMembers = ms,
        remoteMembers = remoteMems,
        protocol = proto,
        metadata = toConvMeta row
      }

toConvMeta :: ConvRow -> ConversationMetadata
toConvMeta (cty, muid, acc, roleV2, nme, ti, timer, rm, _, _, _, _, _, mgct, mAp, mcells, mparent) =
  let accessRoles = maybeRole cty roleV2
   in ConversationMetadata
        { cnvmType = cty,
          cnvmCreator = muid,
          cnvmAccess = defAccess cty acc,
          cnvmAccessRoles = accessRoles,
          cnvmName = nme,
          cnvmTeam = ti,
          cnvmMessageTimer = timer,
          cnvmReceiptMode = rm,
          cnvmGroupConvType = mgct,
          cnvmCellsState = fromMaybe def mcells,
          cnvmChannelAddPermission = mAp,
          cnvmParent = mparent
        }

newStoredConversation :: Local ConvId -> NewConversation -> StoredConversation
newStoredConversation lcnv nc =
  let meta = nc.metadata
      proto = case nc.protocol of
        BaseProtocolProteusTag -> ProtocolProteus
        BaseProtocolMLSTag ->
          let newGid =
                MLS.newGroupId meta.cnvmType $
                  Conv <$> tUntagged lcnv
              gid = fromMaybe newGid nc.groupId
           in ( ProtocolMLS
                  ConversationMLSData
                    { cnvmlsGroupId = gid,
                      cnvmlsActiveData = Nothing
                    }
              )
      UserList lmems rmems = toUserRole <$> nc.users
   in StoredConversation
        { id_ = tUnqualified lcnv,
          localMembers = newMemberWithRole <$> lmems,
          remoteMembers = newRemoteMemberWithRole <$> rmems,
          metadata = meta,
          protocol = proto
        }

newRemoteMemberWithRole :: Remote (UserId, RoleName) -> RemoteMember
newRemoteMemberWithRole ur@(tUntagged -> (Qualified (u, r) _)) =
  RemoteMember
    { id_ = qualifyAs ur u,
      convRoleName = r
    }

convProtocolTag :: StoredConversation -> ProtocolTag
convProtocolTag conv = protocolTag conv.protocol

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
localOne2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
localOne2OneConvId a b = Id . U.unpack $ U.addv4 a b

convType :: StoredConversation -> ConvType
convType c = c.metadata.cnvmType

convSetType :: ConvType -> StoredConversation -> StoredConversation
convSetType t c = c {metadata = c.metadata {cnvmType = t}}

convTeam :: StoredConversation -> Maybe TeamId
convTeam c = c.metadata.cnvmTeam

convAccess :: StoredConversation -> [Access]
convAccess c = c.metadata.cnvmAccess

convAccessRoles :: StoredConversation -> Set AccessRole
convAccessRoles c = c.metadata.cnvmAccessRoles

convAccessData :: StoredConversation -> ConversationAccessData
convAccessData c =
  ConversationAccessData
    (Set.fromList (convAccess c))
    (convAccessRoles c)

convName :: StoredConversation -> Maybe Text
convName c = c.metadata.cnvmName

convSetName :: Maybe Text -> StoredConversation -> StoredConversation
convSetName n c = c {metadata = c.metadata {cnvmName = n}}

defRegularConvAccess :: [Access]
defRegularConvAccess = [InviteAccess]

convMessageTimer :: StoredConversation -> Maybe Milliseconds
convMessageTimer c = c.metadata.cnvmMessageTimer

convReceiptMode :: StoredConversation -> Maybe ReceiptMode
convReceiptMode c = c.metadata.cnvmReceiptMode

data NewConversation = NewConversation
  { metadata :: ConversationMetadata,
    users :: UserList (UserId, RoleName),
    protocol :: BaseProtocolTag,
    groupId :: Maybe GroupId
  }

data MLSMigrationState
  = MLSMigrationMixed
  | MLSMigrationMLS
  deriving (Show, Eq, Ord)

mlsMetadata :: StoredConversation -> Maybe (ConversationMLSData, MLSMigrationState)
mlsMetadata conv =
  case conv.protocol of
    ProtocolProteus -> Nothing
    ProtocolMLS meta -> pure (meta, MLSMigrationMLS)
    ProtocolMixed meta -> pure (meta, MLSMigrationMixed)

-- | Internal (cassandra) representation of a remote conversation member.
data RemoteMember = RemoteMember
  { id_ :: Remote UserId,
    convRoleName :: RoleName
  }
  deriving stock (Eq, Show)

instance Ord RemoteMember where
  compare :: RemoteMember -> RemoteMember -> Ordering
  compare = compare `on` (.id_)

remoteMemberToOther :: RemoteMember -> OtherMember
remoteMemberToOther x =
  OtherMember
    { omQualifiedId = tUntagged x.id_,
      omService = Nothing,
      omConvRoleName = x.convRoleName
    }

remoteMemberQualify :: RemoteMember -> Remote RemoteMember
remoteMemberQualify m = qualifyAs m.id_ m

-- | Internal (cassandra) representation of a local conversation member.
data LocalMember = LocalMember
  { id_ :: UserId,
    status :: MemberStatus,
    service :: Maybe ServiceRef,
    convRoleName :: RoleName
  }
  deriving stock (Show)

newMember :: UserId -> LocalMember
newMember u = newMemberWithRole (u, roleNameWireAdmin)

newMemberWithRole :: (UserId, RoleName) -> LocalMember
newMemberWithRole (u, r) =
  LocalMember
    { id_ = u,
      service = Nothing,
      status = defMemberStatus,
      convRoleName = r
    }

localMemberToOther :: Domain -> LocalMember -> OtherMember
localMemberToOther domain x =
  OtherMember
    { omQualifiedId = Qualified x.id_ domain,
      omService = x.service,
      omConvRoleName = x.convRoleName
    }

data MemberStatus = MemberStatus
  { msOtrMutedStatus :: Maybe MutedStatus,
    msOtrMutedRef :: Maybe Text,
    msOtrArchived :: Bool,
    msOtrArchivedRef :: Maybe Text,
    msHidden :: Bool,
    msHiddenRef :: Maybe Text
  }
  deriving stock (Show)

defMemberStatus :: MemberStatus
defMemberStatus =
  MemberStatus
    { msOtrMutedStatus = Nothing,
      msOtrMutedRef = Nothing,
      msOtrArchived = False,
      msOtrArchivedRef = Nothing,
      msHidden = False,
      msHiddenRef = Nothing
    }

defAccess :: ConvType -> Maybe [Access] -> [Access]
defAccess SelfConv Nothing = [PrivateAccess]
defAccess ConnectConv Nothing = [PrivateAccess]
defAccess One2OneConv Nothing = [PrivateAccess]
defAccess RegularConv Nothing = defRegularConvAccess
defAccess SelfConv (Just []) = [PrivateAccess]
defAccess ConnectConv (Just []) = [PrivateAccess]
defAccess One2OneConv (Just []) = [PrivateAccess]
defAccess RegularConv (Just []) = defRegularConvAccess
defAccess _ (Just xs@(_ : _)) = xs

-- BotMember ------------------------------------------------------------------

-- | For now we assume bots to always be local
--
-- FUTUREWORK(federation): allow remote bots
newtype BotMember = BotMember {fromBotMember :: LocalMember} deriving (Show)

instance Eq BotMember where
  (==) = (==) `on` botMemId

instance Ord BotMember where
  compare = compare `on` botMemId

newBotMember :: LocalMember -> Maybe BotMember
newBotMember m = BotMember m <$ m.service

botMemId :: BotMember -> BotId
botMemId m = BotId $ m.fromBotMember.id_

botMemService :: BotMember -> ServiceRef
botMemService m = fromJust $ m.fromBotMember.service

localBotsAndUsers :: (Foldable f) => f LocalMember -> ([BotMember], [LocalMember])
localBotsAndUsers = foldMap botOrUser
  where
    botOrUser m = case m.service of
      -- we drop invalid bots here, which shouldn't happen
      Just _ -> (toList (newBotMember m), [])
      Nothing -> ([], [m])

nonTeamMembers :: [LocalMember] -> [TeamMember] -> [LocalMember]
nonTeamMembers cm tm = filter (not . isMemberOfTeam . (.id_)) cm
  where
    -- FUTUREWORK: remote members: teams and their members are always on the same backend
    isMemberOfTeam = \case
      uid -> isTeamMember uid tm
