{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.StoredConversation where

import Cassandra qualified as C
import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Data.UUID.Tagged qualified as U
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Provider.Service
import Wire.API.User
import Wire.UserList

-- | Internal conversation type, corresponding directly to database schema.
-- Should never be sent to users (and therefore doesn't have 'FromJSON' or
-- 'ToJSON' instances).
data StoredConversation = StoredConversation
  { id_ :: ConvId,
    localMembers :: [LocalMember],
    remoteMembers :: [RemoteMember],
    deleted :: Bool,
    metadata :: ConversationMetadata,
    protocol :: Protocol
  }
  deriving (Show)

convProtocolTag :: StoredConversation -> ProtocolTag
convProtocolTag conv = protocolTag conv.protocol

isConvDeleted :: StoredConversation -> Bool
isConvDeleted = (.deleted)

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

parseAccessRoles :: Maybe AccessRoleLegacy -> Maybe (Set AccessRole) -> Maybe (Set AccessRole)
parseAccessRoles mbLegacy mbAccess = mbAccess <|> fromAccessRoleLegacy <$> mbLegacy

convMessageTimer :: StoredConversation -> Maybe Milliseconds
convMessageTimer c = c.metadata.cnvmMessageTimer

convReceiptMode :: StoredConversation -> Maybe ReceiptMode
convReceiptMode c = c.metadata.cnvmReceiptMode

data NewConversation = NewConversation
  { metadata :: ConversationMetadata,
    users :: UserList (UserId, RoleName),
    protocol :: BaseProtocolTag
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

defAccess :: ConvType -> Maybe (C.Set Access) -> [Access]
defAccess SelfConv Nothing = [PrivateAccess]
defAccess ConnectConv Nothing = [PrivateAccess]
defAccess One2OneConv Nothing = [PrivateAccess]
defAccess RegularConv Nothing = defRegularConvAccess
defAccess SelfConv (Just (C.Set [])) = [PrivateAccess]
defAccess ConnectConv (Just (C.Set [])) = [PrivateAccess]
defAccess One2OneConv (Just (C.Set [])) = [PrivateAccess]
defAccess RegularConv (Just (C.Set [])) = defRegularConvAccess
defAccess _ (Just (C.Set (x : xs))) = x : xs

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
