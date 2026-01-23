module Wire.ConversationSubsystem.View where

import Data.Domain (Domain)
import Data.Id (UserId, idToText)
import Data.Qualified
import Galley.Types.Error (InternalError (BadMemberState))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import System.Logger.Message (msg, val, (+++))
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation qualified as Conversation
import Wire.API.Federation.API.Galley
import Wire.StoredConversation

conversationViewV9 ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  StoredConversation ->
  Sem r OwnConversation
conversationViewV9 luid conv = do
  let remoteOthers = map remoteMemberToOther $ conv.remoteMembers
      localOthers = map (localMemberToOther (tDomain luid)) $ conv.localMembers
  conversationViewWithCachedOthers remoteOthers localOthers conv luid

conversationView ::
  Local x ->
  Maybe (Local UserId) ->
  StoredConversation ->
  Conversation
conversationView l luid conv =
  let remoteMembers = map remoteMemberToOther $ conv.remoteMembers
      localMembers = map (localMemberToOther (tDomain l)) $ conv.localMembers
      selfs = filter (\m -> fmap tUnqualified luid == Just m.id_) (conv.localMembers)
      mSelf = localMemberToSelf l <$> listToMaybe selfs
      others = filter (\oth -> (tUntagged <$> luid) /= Just (omQualifiedId oth)) localMembers <> remoteMembers
   in Conversation
        { members = ConvMembers mSelf others,
          qualifiedId = (tUntagged . qualifyAs l $ conv.id_),
          metadata = conv.metadata,
          protocol = conv.protocol
        }

conversationViewWithCachedOthers ::
  ( Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  [OtherMember] ->
  [OtherMember] ->
  StoredConversation ->
  Local UserId ->
  Sem r OwnConversation
conversationViewWithCachedOthers remoteOthers localOthers conv luid = do
  let mbConv = conversationViewMaybe luid remoteOthers localOthers conv
  maybe memberNotFound pure mbConv
  where
    memberNotFound = do
      P.err . msg $
        val "User "
          +++ idToText (tUnqualified luid)
          +++ val " is not a member of conv "
          +++ idToText conv.id_
      throw BadMemberState

conversationViewMaybe :: Local UserId -> [OtherMember] -> [OtherMember] -> StoredConversation -> Maybe OwnConversation
conversationViewMaybe luid remoteOthers localOthers conv = do
  let selfs = filter (\m -> tUnqualified luid == m.id_) conv.localMembers
  self <- localMemberToSelf luid <$> listToMaybe selfs
  let others = filter (\oth -> tUntagged luid /= omQualifiedId oth) localOthers <> remoteOthers
  pure $
    OwnConversation
      (tUntagged . qualifyAs luid $ conv.id_)
      conv.metadata
      (OwnConvMembers self others)
      conv.protocol

remoteConversationView ::
  Local UserId ->
  MemberStatus ->
  Remote RemoteConversationV2 ->
  OwnConversation
remoteConversationView uid status (tUntagged -> Qualified rconv rDomain) =
  let mems = rconv.members
      others = mems.others
      self =
        localMemberToSelf
          uid
          LocalMember
            { id_ = tUnqualified uid,
              service = Nothing,
              status = status,
              convRoleName = mems.selfRole
            }
   in OwnConversation
        (Qualified rconv.id rDomain)
        rconv.metadata
        (OwnConvMembers self others)
        rconv.protocol

conversationToRemote ::
  Domain ->
  Remote UserId ->
  StoredConversation ->
  Maybe RemoteConversationV2
conversationToRemote localDomain ruid conv = do
  let (selfs, rothers) = partition (\r -> r.id_ == ruid) (conv.remoteMembers)
      lothers = conv.localMembers
  selfRole' <- (.convRoleName) <$> listToMaybe selfs
  let others' =
        map (localMemberToOther localDomain) lothers
          <> map remoteMemberToOther rothers
  pure $
    RemoteConversationV2
      { id = conv.id_,
        metadata = conv.metadata,
        members =
          RemoteConvMembers
            { selfRole = selfRole',
              others = others'
            },
        protocol = conv.protocol
      }

localMemberToSelf :: Local x -> LocalMember -> Conversation.Member
localMemberToSelf loc lm =
  Conversation.Member
    { memId = tUntagged . qualifyAs loc $ lm.id_,
      memService = lm.service,
      memOtrMutedStatus = msOtrMutedStatus st,
      memOtrMutedRef = msOtrMutedRef st,
      memOtrArchived = msOtrArchived st,
      memOtrArchivedRef = msOtrArchivedRef st,
      memHidden = msHidden st,
      memHiddenRef = msHiddenRef st,
      memConvRoleName = lm.convRoleName
    }
  where
    st = lm.status
