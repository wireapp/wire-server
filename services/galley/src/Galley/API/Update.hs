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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Galley.API.Update
  ( -- * Managing Conversations
    acceptConv,
    blockConv,
    blockConvUnqualified,
    unblockConv,
    unblockConvUnqualified,
    checkReusableCode,
    joinConversationByReusableCode,
    joinConversationById,
    addCodeUnqualified,
    addCodeUnqualifiedWithReqBody,
    rmCodeUnqualified,
    getCode,
    updateUnqualifiedConversationName,
    updateConversationName,
    updateConversationReceiptModeUnqualified,
    updateConversationReceiptMode,
    updateConversationMessageTimerUnqualified,
    updateConversationMessageTimer,
    updateConversationAccessUnqualified,
    updateConversationAccess,
    updateChannelAddPermission,
    deleteLocalConversation,
    updateRemoteConversation,
    updateConversationProtocolWithLocalUser,
    updateLocalStateOfRemoteConv,
    updateCellsState,

    -- * Managing Members
    addMembersUnqualified,
    addMembersUnqualifiedV2,
    addMembers,
    updateUnqualifiedSelfMember,
    updateSelfMember,
    updateOtherMember,
    updateOtherMemberUnqualified,
    removeMemberQualified,
    removeMemberUnqualified,
    removeMemberFromLocalConv,
    removeMemberFromRemoteConv,

    -- * Talking
    postProteusMessage,
    postOtrMessageUnqualified,
    postProteusBroadcast,
    postOtrBroadcastUnqualified,
    memberTypingUnqualified,
    memberTyping,

    -- * External Services
    addBot,
    rmBot,
    postBotMessageUnqualified,
  )
where

import Control.Error.Util (hush)
import Control.Lens
import Data.Code
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Map.Strict qualified as Map
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons
import Data.Time
import Galley.API.Action
import Galley.API.Cells
import Galley.API.Error
import Galley.API.MLS.Removal (RemoveUserIncludeMain (RemoveUserIncludeMain), removeUser)
import Galley.API.Mapping
import Galley.API.Message
import Galley.API.Query qualified as Query
import Galley.API.Teams.Features.Get
import Galley.API.Util
import Galley.App
import Galley.Data.Conversation qualified as Data
import Galley.Data.Conversation.Types qualified as Data
import Galley.Data.Services as Data
import Galley.Data.Types hiding (Conversation)
import Galley.Effects
import Galley.Effects.ClientStore qualified as E
import Galley.Effects.CodeStore qualified as E
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.ExternalAccess qualified as E
import Galley.Effects.FederatorAccess qualified as E
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.TeamStore qualified as E
import Galley.Options
import Galley.Types.Conversations.Members (LocalMember (..), RemoteMember (..))
import Galley.Types.UserList
import Imports hiding (forkIO)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Bot hiding (addBot)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Code
import Wire.API.Conversation.Protocol qualified as P
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Message
import Wire.API.Routes.Public (ZHostValue)
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.Routes.Public.Util (UpdateResult (..))
import Wire.API.ServantProto (RawProto (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.User.Client
import Wire.HashPassword as HashPassword
import Wire.NotificationSubsystem
import Wire.RateLimit

acceptConv ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r ConversationV8
acceptConv lusr conn cnv = do
  conv <-
    E.getConversation cnv >>= noteS @'ConvNotFound
  conv' <- acceptOne2One lusr conv conn
  conversationViewV8 lusr conv'

blockConv ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member MemberStore r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r ()
blockConv lusr qcnv =
  foldQualified
    lusr
    (blockConvUnqualified (tUnqualified lusr) . tUnqualified)
    (blockRemoteConv lusr)
    qcnv

blockConvUnqualified ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member MemberStore r
  ) =>
  UserId ->
  ConvId ->
  Sem r ()
blockConvUnqualified zusr cnv = do
  conv <- E.getConversation cnv >>= noteS @'ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwS @'InvalidOperation
  let mems = Data.convLocalMembers conv
  when (zusr `isMember` mems) $
    E.deleteMembers cnv (UserList [zusr] [])

blockRemoteConv ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member MemberStore r
  ) =>
  Local UserId ->
  Remote ConvId ->
  Sem r ()
blockRemoteConv (tUnqualified -> usr) rcnv = do
  unlessM (E.checkLocalMemberRemoteConv usr rcnv) $ throwS @'ConvNotFound
  E.deleteMembersInRemoteConversation rcnv [usr]

unblockConv ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  Sem r ()
unblockConv lusr conn =
  foldQualified
    lusr
    (void . unblockConvUnqualified lusr conn . tUnqualified)
    (unblockRemoteConv lusr)

unblockConvUnqualified ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r ConversationV8
unblockConvUnqualified lusr conn cnv = do
  conv <-
    E.getConversation cnv >>= noteS @'ConvNotFound
  unless (Data.convType conv `elem` [ConnectConv, One2OneConv]) $
    throwS @'InvalidOperation
  conv' <- acceptOne2One lusr conv conn
  conversationViewV8 lusr conv'

unblockRemoteConv ::
  ( Member MemberStore r
  ) =>
  Local UserId ->
  Remote ConvId ->
  Sem r ()
unblockRemoteConv lusr rcnv = do
  E.createMembersInRemoteConversation rcnv [tUnqualified lusr]

-- conversation updates

type UpdateConversationAccessEffects =
  '[ BackendNotificationQueueAccess,
     BotAccess,
     BrigAccess,
     CodeStore,
     ConversationStore,
     Error FederationError,
     Error InternalError,
     Error InvalidInput,
     ErrorS ('ActionDenied 'ModifyConversationAccess),
     ErrorS ('ActionDenied 'RemoveConversationMember),
     ErrorS 'ConvNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'InvalidTargetAccess,
     ExternalAccess,
     FederatorAccess,
     FireAndForget,
     NotificationSubsystem,
     Input Env,
     Input UTCTime,
     MemberStore,
     ProposalStore,
     Random,
     SubConversationStore,
     TeamStore,
     TinyLog
   ]

updateConversationAccess ::
  ( Members UpdateConversationAccessEffects r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccess lusr con qcnv update = do
  lcnv <- ensureLocal lusr qcnv
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationAccessDataTag lcnv (tUntagged lusr) (Just con) update

updateConversationAccessUnqualified ::
  ( Members UpdateConversationAccessEffects r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationAccessData ->
  Sem r (UpdateResult Event)
updateConversationAccessUnqualified lusr con cnv update =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationAccessDataTag
      (qualifyAs lusr cnv)
      (tUntagged lusr)
      (Just con)
      update

updateConversationReceiptMode ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'ModifyConversationReceiptMode)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'MLSReadReceiptsNotAllowed) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TinyLog r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptMode lusr zcon qcnv update =
  mapError @UnreachableBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected UnreachableBackends error when updating remote receipt mode")
    . mapError @NonFederatingBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected NonFederatingBackends error when updating remote receipt mode")
    $ foldQualified
      lusr
      ( \lcnv ->
          getUpdateResult . fmap lcuEvent $
            updateLocalConversation
              @'ConversationReceiptModeUpdateTag
              lcnv
              (tUntagged lusr)
              (Just zcon)
              update
      )
      (\rcnv -> updateRemoteConversation @'ConversationReceiptModeUpdateTag rcnv lusr (Just zcon) update)
      qcnv

updateRemoteConversation ::
  forall tag r.
  ( Member BrigAccess r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member MemberStore r,
    Member TinyLog r,
    RethrowErrors (HasConversationActionGalleyErrors tag) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member (Error FederationError) r,
    SingI tag
  ) =>
  Remote ConvId ->
  Local UserId ->
  Maybe ConnId ->
  ConversationAction tag ->
  Sem r (UpdateResult Event)
updateRemoteConversation rcnv lusr mconn action = getUpdateResult $ do
  let updateRequest =
        ConversationUpdateRequest
          { user = tUnqualified lusr,
            convId = tUnqualified rcnv,
            action = SomeConversationAction (sing @tag) action
          }
  eResponse <- E.runFederatedEither rcnv (fedClient @'Galley @"update-conversation" updateRequest)
  response <- case eResponse of
    Left e -> throw e
    Right x -> pure x
  convUpdate <- case response of
    ConversationUpdateResponseNoChanges -> throw NoChanges
    ConversationUpdateResponseError err' -> raise $ rethrowErrors @(HasConversationActionGalleyErrors tag) err'
    ConversationUpdateResponseUpdate convUpdate -> pure convUpdate
    ConversationUpdateResponseNonFederatingBackends e -> throw e
    ConversationUpdateResponseUnreachableBackends e -> throw e
  updateLocalStateOfRemoteConv (qualifyAs rcnv convUpdate) mconn >>= note NoChanges

updateConversationReceiptModeUnqualified ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'ModifyConversationReceiptMode)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'MLSReadReceiptsNotAllowed) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TinyLog r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationReceiptModeUpdate ->
  Sem r (UpdateResult Event)
updateConversationReceiptModeUnqualified lusr zcon cnv = updateConversationReceiptMode lusr zcon (tUntagged (qualifyAs lusr cnv))

updateConversationMessageTimer ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (ErrorS ('ActionDenied 'ModifyConversationMessageTimer)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimer lusr zcon qcnv update =
  getUpdateResult $
    foldQualified
      lusr
      ( \lcnv ->
          lcuEvent
            <$> updateLocalConversation
              @'ConversationMessageTimerUpdateTag
              lcnv
              (tUntagged lusr)
              (Just zcon)
              update
      )
      (\_ -> throw FederationNotImplemented)
      qcnv

updateConversationMessageTimerUnqualified ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (ErrorS ('ActionDenied 'ModifyConversationMessageTimer)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationMessageTimerUpdate ->
  Sem r (UpdateResult Event)
updateConversationMessageTimerUnqualified lusr zcon cnv = updateConversationMessageTimer lusr zcon (tUntagged (qualifyAs lusr cnv))

deleteLocalConversation ::
  ( Member BrigAccess r,
    Member BackendNotificationQueueAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS ('ActionDenied 'DeleteConversation)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member SubConversationStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r (UpdateResult Event)
deleteLocalConversation lusr con lcnv =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationDeleteTag lcnv (tUntagged lusr) (Just con) ()

addCodeUnqualifiedWithReqBody ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'CreateConversationCodeConflict) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member HashPassword r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member RateLimit r,
    Member TeamStore r
  ) =>
  UserId ->
  Maybe Text ->
  Maybe ConnId ->
  ConvId ->
  CreateConversationCodeRequest ->
  Sem r AddCodeResult
addCodeUnqualifiedWithReqBody usr mbZHost mZcon cnv req = addCodeUnqualified (Just req) usr mbZHost mZcon cnv

addCodeUnqualified ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'CreateConversationCodeConflict) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member HashPassword r,
    Member TeamFeatureStore r,
    Member RateLimit r,
    Member TeamStore r
  ) =>
  Maybe CreateConversationCodeRequest ->
  UserId ->
  Maybe ZHostValue ->
  Maybe ConnId ->
  ConvId ->
  Sem r AddCodeResult
addCodeUnqualified mReq usr mbZHost mZcon cnv = do
  lusr <- qualifyLocal usr
  lcnv <- qualifyLocal cnv
  addCode lusr mbZHost mZcon lcnv mReq

addCode ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'CreateConversationCodeConflict) r,
    Member ExternalAccess r,
    Member HashPassword r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member RateLimit r,
    Member TeamStore r
  ) =>
  Local UserId ->
  Maybe ZHostValue ->
  Maybe ConnId ->
  Local ConvId ->
  Maybe CreateConversationCodeRequest ->
  Sem r AddCodeResult
addCode lusr mbZHost mZcon lcnv mReq = do
  conv <- E.getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled (Data.convTeam conv)
  mTeamMember <- maybe (pure Nothing) (flip E.getTeamMember (tUnqualified lusr)) conv.convMetadata.cnvmTeam
  Query.ensureConvAdmin conv (tUnqualified lusr) mTeamMember
  ensureAccess conv CodeAccess
  ensureGuestsOrNonTeamMembersAllowed conv
  convUri <- getConversationCodeURI mbZHost
  key <- E.makeKey (tUnqualified lcnv)
  E.getCode key ReusableCode >>= \case
    Nothing -> do
      ttl <- realToFrac . unGuestLinkTTLSeconds . fromMaybe defGuestLinkTTLSeconds . view (settings . guestLinkTTLSeconds) <$> input
      code <- E.generateCode (tUnqualified lcnv) ReusableCode (Timeout ttl)
      mPw <- for (mReq >>= (.password)) $ HashPassword.hashPassword8 (RateLimitUser (tUnqualified lusr))
      E.createCode code mPw
      now <- input
      let event = Event (tUntagged lcnv) Nothing (tUntagged lusr) now Nothing (EdConvCodeUpdate (mkConversationCodeInfo (isJust mPw) (codeKey code) (codeValue code) convUri))
      let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
      pushConversationEvent mZcon conv event (qualifyAs lusr (map lmId users)) bots
      pure $ CodeAdded event
    -- In case conversation already has a code this case covers the allowed no-ops
    Just (code, mPw) -> do
      when (isJust mPw || isJust (mReq >>= (.password))) $ throwS @'CreateConversationCodeConflict
      pure $ CodeAlreadyExisted (mkConversationCodeInfo (isJust mPw) (codeKey code) (codeValue code) convUri)
  where
    ensureGuestsOrNonTeamMembersAllowed :: Data.Conversation -> Sem r ()
    ensureGuestsOrNonTeamMembersAllowed conv =
      unless
        ( GuestAccessRole `Set.member` Data.convAccessRoles conv
            || NonTeamMemberAccessRole `Set.member` Data.convAccessRoles conv
        )
        $ throwS @'ConvAccessDenied

rmCodeUnqualified ::
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Sem r Event
rmCodeUnqualified lusr zcon cnv = do
  lcnv <- qualifyLocal cnv
  rmCode lusr zcon lcnv

rmCode ::
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  Sem r Event
rmCode lusr zcon lcnv = do
  conv <-
    E.getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound
  mTeamMember <- maybe (pure Nothing) (flip E.getTeamMember (tUnqualified lusr)) conv.convMetadata.cnvmTeam
  Query.ensureConvAdmin conv (tUnqualified lusr) mTeamMember
  ensureAccess conv CodeAccess
  let (bots, users) = localBotsAndUsers $ Data.convLocalMembers conv
  key <- E.makeKey (tUnqualified lcnv)
  E.deleteCode key ReusableCode
  now <- input
  let event = Event (tUntagged lcnv) Nothing (tUntagged lusr) now Nothing EdConvCodeDelete
  pushConversationEvent (Just zcon) conv event (qualifyAs lusr (map lmId users)) bots
  pure event

getCode ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Maybe ZHostValue ->
  Local UserId ->
  ConvId ->
  Sem r ConversationCodeInfo
getCode mbZHost lusr cnv = do
  conv <-
    E.getConversation cnv >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled (Data.convTeam conv)
  ensureAccess conv CodeAccess
  ensureConvMember (Data.convLocalMembers conv) (tUnqualified lusr)
  key <- E.makeKey cnv
  (c, mPw) <- E.getCode key ReusableCode >>= noteS @'CodeNotFound
  convUri <- getConversationCodeURI mbZHost
  pure $ mkConversationCodeInfo (isJust mPw) (codeKey c) (codeValue c) convUri

checkReusableCode ::
  forall r.
  ( Member CodeStore r,
    Member ConversationStore r,
    Member TeamFeatureStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidConversationPassword) r,
    Member (Input Opts) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  IpAddr ->
  ConversationCode ->
  Sem r ()
checkReusableCode origIp convCode = do
  code <- verifyReusableCode (RateLimitIp origIp) False Nothing convCode
  conv <- E.getConversation (codeConversation code) >>= noteS @'ConvNotFound
  mapErrorS @'GuestLinksDisabled @'CodeNotFound $
    Query.ensureGuestLinksEnabled (Data.convTeam conv)

updateConversationProtocolWithLocalUser ::
  forall r.
  ( Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvInvalidProtocolTransition) r,
    Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSMigrationCriteriaNotSatisfied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Error InternalError) r,
    Member (Input UTCTime) r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member MemberStore r,
    Member TinyLog r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member Random r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  P.ProtocolUpdate ->
  Sem r (UpdateResult Event)
updateConversationProtocolWithLocalUser lusr conn qcnv (P.ProtocolUpdate newProtocol) =
  mapError @UnreachableBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected UnreachableBackends error when updating remote protocol")
    . mapError @NonFederatingBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected NonFederatingBackends error when updating remote protocol")
    $ foldQualified
      lusr
      ( \lcnv -> do
          fmap (maybe Unchanged (Updated . lcuEvent) . hush)
            . runError @NoChanges
            . updateLocalConversation @'ConversationUpdateProtocolTag lcnv (tUntagged lusr) (Just conn)
            $ newProtocol
      )
      ( \rcnv ->
          updateRemoteConversation @'ConversationUpdateProtocolTag rcnv lusr (Just conn) $
            newProtocol
      )
      qcnv

updateChannelAddPermission ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (ErrorS ('ActionDenied 'ModifyAddPermission)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member (ErrorS (MissingPermission Nothing)) r,
    Member (ErrorS NotATeamMember) r,
    Member (ErrorS TeamNotFound) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member BrigAccess r,
    Member FederatorAccess r,
    Member MemberStore r,
    Member (ErrorS 'InvalidTargetAccess) r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  AddPermissionUpdate ->
  Sem r (UpdateResult Event)
updateChannelAddPermission lusr zcon qcnv update =
  foldQualified
    lusr
    ( \lcnv ->
        getUpdateResult $
          lcuEvent
            <$> updateLocalConversation
              @'ConversationUpdateAddPermissionTag
              lcnv
              (tUntagged lusr)
              (Just zcon)
              update
    )
    (\rcnv -> updateRemoteConversation @'ConversationUpdateAddPermissionTag rcnv lusr (Just zcon) update)
    qcnv

joinConversationByReusableCode ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'InvalidConversationPassword) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  Local UserId ->
  ConnId ->
  JoinConversationByCode ->
  Sem r (UpdateResult Event)
joinConversationByReusableCode lusr zcon req = do
  c <- verifyReusableCode (RateLimitUser (tUnqualified lusr)) True req.password req.code
  conv <- E.getConversation (codeConversation c) >>= noteS @'ConvNotFound
  Query.ensureGuestLinksEnabled (Data.convTeam conv)
  joinConversation lusr zcon conv CodeAccess

joinConversationById ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Sem r (UpdateResult Event)
joinConversationById lusr zcon cnv = do
  conv <- E.getConversation cnv >>= noteS @'ConvNotFound
  joinConversation lusr zcon conv LinkAccess

joinConversation ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Data.Conversation ->
  Access ->
  Sem r (UpdateResult Event)
joinConversation lusr zcon conv access = do
  let lcnv = qualifyAs lusr conv.convId
  ensureConversationAccess (tUnqualified lusr) conv access
  ensureGroupConversation conv
  -- FUTUREWORK: remote users?
  ensureMemberLimit (Data.convProtocolTag conv) (toList $ Data.convLocalMembers conv) [tUnqualified lusr]
  getUpdateResult $ do
    -- NOTE: When joining conversations, all users become members
    -- as this is our desired behavior for these types of conversations
    -- where there is no way to control who joins, etc.
    let users = filter (notIsConvMember lusr conv) [tUnqualified lusr]
    (extraTargets, action) <-
      addMembersToLocalConversation lcnv (UserList users []) roleNameWireMember InternalAdd
    lcuEvent
      <$> notifyConversationAction
        (sing @'ConversationJoinTag)
        (tUntagged lusr)
        False
        (Just zcon)
        (qualifyAs lusr conv)
        (convBotsAndMembers conv <> extraTargets)
        action

addMembers ::
  forall r.
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
    Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (Error FederationError) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  InviteQualified ->
  Sem r (UpdateResult Event)
addMembers lusr zcon qcnv (InviteQualified users role) = do
  lcnv <- ensureLocal lusr qcnv
  conv <- getConversationWithError lcnv
  let joinType = if notIsConvMember lusr conv (tUntagged lusr) then ExternalAdd else InternalAdd
  let action = ConversationJoin users role joinType
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationJoinTag lcnv (tUntagged lusr) (Just zcon) action

addMembersUnqualifiedV2 ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
    Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  InviteQualified ->
  Sem r (UpdateResult Event)
addMembersUnqualifiedV2 lusr zcon cnv (InviteQualified users role) = do
  let lcnv = qualifyAs lusr cnv
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationJoinTag lcnv (tUntagged lusr) (Just zcon) $
      ConversationJoin users role def

addMembersUnqualified ::
  ( Member BackendNotificationQueueAccess r,
    Member BrigAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
    Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TooManyMembers) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member (Error NonFederatingBackends) r,
    Member (Error UnreachableBackends) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Invite ->
  Sem r (UpdateResult Event)
addMembersUnqualified lusr zcon cnv (Invite users role) = do
  let qusers = fmap (tUntagged . qualifyAs lusr) (toNonEmpty users)
  addMembers lusr zcon (tUntagged (qualifyAs lusr cnv)) (InviteQualified qusers role)

updateSelfMember ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  MemberUpdate ->
  Sem r ()
updateSelfMember lusr zcon qcnv update = do
  exists <- foldQualified lusr checkLocalMembership checkRemoteMembership qcnv
  unless exists $ throwS @'ConvNotFound
  E.setSelfMember qcnv lusr update
  now <- input
  let e = Event qcnv Nothing (tUntagged lusr) now Nothing (EdMemberUpdate (updateData lusr))
  pushConversationEvent (Just zcon) () e (fmap pure lusr) []
  where
    checkLocalMembership ::
      (Member MemberStore r) =>
      Local ConvId ->
      Sem r Bool
    checkLocalMembership lcnv =
      isMember (tUnqualified lusr)
        <$> E.getLocalMembers (tUnqualified lcnv)
    checkRemoteMembership ::
      (Member ConversationStore r) =>
      Remote ConvId ->
      Sem r Bool
    checkRemoteMembership rcnv =
      isJust . Map.lookup rcnv
        <$> E.getRemoteConversationStatus (tUnqualified lusr) [rcnv]
    updateData luid =
      MemberUpdateData
        { misTarget = tUntagged luid,
          misOtrMutedStatus = mupOtrMuteStatus update,
          misOtrMutedRef = mupOtrMuteRef update,
          misOtrArchived = mupOtrArchive update,
          misOtrArchivedRef = mupOtrArchiveRef update,
          misHidden = mupHidden update,
          misHiddenRef = mupHiddenRef update,
          misConvRoleName = Nothing
        }

updateUnqualifiedSelfMember ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  MemberUpdate ->
  Sem r ()
updateUnqualifiedSelfMember lusr zcon cnv update = do
  let lcnv = qualifyAs lusr cnv
  updateSelfMember lusr zcon (tUntagged lcnv) update

updateOtherMemberLocalConv ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied 'ModifyOtherConversationMember)) r,
    Member (ErrorS 'InvalidTarget) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberLocalConv lcnv lusr con qvictim update = void . getUpdateResult . fmap lcuEvent $ do
  when (tUntagged lusr == qvictim) $
    throwS @'InvalidTarget
  updateLocalConversation @'ConversationMemberUpdateTag lcnv (tUntagged lusr) (Just con) $
    ConversationMemberUpdate qvictim update

updateOtherMemberUnqualified ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied 'ModifyOtherConversationMember)) r,
    Member (ErrorS 'InvalidTarget) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberUnqualified lusr zcon cnv victim update = do
  let lcnv = qualifyAs lusr cnv
  let lvictim = qualifyAs lusr victim
  updateOtherMemberLocalConv lcnv lusr zcon (tUntagged lvictim) update

updateOtherMember ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS ('ActionDenied 'ModifyOtherConversationMember)) r,
    Member (ErrorS 'InvalidTarget) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvMemberNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMember lusr zcon qcnv qvictim update = do
  let doUpdate = foldQualified lusr updateOtherMemberLocalConv updateOtherMemberRemoteConv
  doUpdate qcnv lusr zcon qvictim update

updateOtherMemberRemoteConv ::
  (Member (Error FederationError) r) =>
  Remote ConvId ->
  Local UserId ->
  ConnId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Sem r ()
updateOtherMemberRemoteConv _ _ _ _ _ = throw FederationNotImplemented

removeMemberUnqualified ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  UserId ->
  Sem r (Maybe Event)
removeMemberUnqualified lusr con cnv victim = do
  let lvictim = qualifyAs lusr victim
      lcnv = qualifyAs lusr cnv
  removeMemberQualified lusr con (tUntagged lcnv) (tUntagged lvictim)

removeMemberQualified ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberQualified lusr con qcnv victim =
  mapErrorS @('ActionDenied 'LeaveConversation) @('ActionDenied 'RemoveConversationMember) $
    foldQualified
      lusr
      (\lcnv -> removeMemberFromLocalConv lcnv lusr (Just con))
      (\rcnv -> removeMemberFromRemoteConv rcnv lusr)
      qcnv
      victim

-- | if the public member leave api was called, we can assume that
--   it was called by a user
pattern EdMembersLeaveRemoved :: QualifiedUserIdList -> EventData
pattern EdMembersLeaveRemoved l = EdMembersLeave EdReasonRemoved l

removeMemberFromRemoteConv ::
  ( Member FederatorAccess r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Input UTCTime) r
  ) =>
  Remote ConvId ->
  Local UserId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberFromRemoteConv cnv lusr victim
  | tUntagged lusr == victim = do
      let lc = LeaveConversationRequest (tUnqualified cnv) (qUnqualified victim)
      let rpc = fedClient @'Galley @"leave-conversation" lc
      E.runFederated cnv rpc
        >>= either handleError handleSuccess . void . (.response)
  | otherwise = throwS @('ActionDenied 'RemoveConversationMember)
  where
    handleError ::
      ( Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
        Member (ErrorS 'ConvNotFound) r
      ) =>
      RemoveFromConversationError ->
      Sem r (Maybe Event)
    handleError RemoveFromConversationErrorRemovalNotAllowed =
      throwS @('ActionDenied 'RemoveConversationMember)
    handleError RemoveFromConversationErrorNotFound = throwS @'ConvNotFound
    handleError RemoveFromConversationErrorUnchanged = pure Nothing

    handleSuccess :: (Member (Input UTCTime) r) => () -> Sem r (Maybe Event)
    handleSuccess _ = do
      t <- input
      pure . Just $
        Event (tUntagged cnv) Nothing (tUntagged lusr) t Nothing $
          EdMembersLeaveRemoved (QualifiedUserIdList [victim])

-- | Remove a member from a local conversation.
removeMemberFromLocalConv ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS ('ActionDenied 'LeaveConversation)) r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r,
    Member TeamStore r
  ) =>
  Local ConvId ->
  Local UserId ->
  Maybe ConnId ->
  Qualified UserId ->
  Sem r (Maybe Event)
removeMemberFromLocalConv lcnv lusr con victim
  | tUntagged lusr == victim =
      fmap (fmap lcuEvent . hush)
        . runError @NoChanges
        . updateLocalConversation @'ConversationLeaveTag lcnv (tUntagged lusr) con
        $ ()
  | otherwise = do
      conv <- getConversationWithError lcnv
      let lconv = qualifyAs lusr conv
      if not (isConvMemberL lconv lusr) && conv.convMetadata.cnvmGroupConvType == Just Channel
        then
          fmap (const Nothing) . runError @NoChanges $ removeMemberFromChannel (tUntagged lusr) lconv victim
        else
          fmap (fmap lcuEvent . hush)
            . runError @NoChanges
            $ updateLocalConversation @'ConversationRemoveMembersTag
              lcnv
              (tUntagged lusr)
              con
              (ConversationRemoveMembers (pure victim) EdReasonRemoved)

removeMemberFromChannel ::
  forall r.
  ( Member (ErrorS 'ConvNotFound) r,
    Member TeamStore r,
    Member (Input Env) r,
    Member MemberStore r,
    Member (Error NoChanges) r,
    Member SubConversationStore r,
    Member ProposalStore r,
    Member (Input UTCTime) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Error InternalError) r,
    Member Random r,
    Member TinyLog r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r
  ) =>
  Qualified UserId ->
  Local Data.Conversation ->
  Qualified UserId ->
  Sem r ()
removeMemberFromChannel qusr lconv victim = do
  let conv = tUnqualified lconv
  mTeamMember <- foldQualified lconv (getTeamMembership conv) (const $ pure Nothing) qusr
  self :: ConvOrTeamMember (Either LocalMember RemoteMember) <- noteS @'ConvNotFound $ TeamMember <$> mTeamMember
  let action = ConversationRemoveMembers {crmTargets = pure victim, crmReason = EdReasonRemoved}
  ensureAllowed @'ConversationRemoveMembersTag (sing @'ConversationRemoveMembersTag) lconv action conv self
  removeUser lconv RemoveUserIncludeMain victim
  where
    getTeamMembership :: Data.Conversation -> Local UserId -> Sem r (Maybe TeamMember)
    getTeamMembership conv luid = maybe (pure Nothing) (`E.getTeamMember` tUnqualified luid) conv.convMetadata.cnvmTeam

-- OTR

postProteusMessage ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  RawProto QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postProteusMessage sender zcon conv msg = runLocalInput sender $ do
  foldQualified
    sender
    (\c -> postQualifiedOtrMessage User (tUntagged sender) (Just zcon) c (rpValue msg))
    (\c -> postRemoteOtrMessage sender c (rpRaw msg))
    conv

postProteusBroadcast ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'BroadcastLimitExceeded) r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  QualifiedNewOtrMessage ->
  Sem r (PostOtrResponse MessageSendingStatus)
postProteusBroadcast sender zcon = postBroadcast sender (Just zcon)

unqualifyEndpoint ::
  (Functor f) =>
  Local x ->
  (QualifiedNewOtrMessage -> f (PostOtrResponse MessageSendingStatus)) ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  f (PostOtrResponse ClientMismatch)
unqualifyEndpoint loc f ignoreMissing reportMissing message = do
  let qualifiedRecipients =
        QualifiedOtrRecipients
          . QualifiedUserClientMap
          . Map.singleton (tDomain loc)
          . userClientMap
          . fmap fromBase64TextLenient
          . otrRecipientsMap
          . newOtrRecipients
          $ message
      clientMismatchStrategy = legacyClientMismatchStrategy (tDomain loc) (newOtrReportMissing message) ignoreMissing reportMissing
      qualifiedMessage =
        QualifiedNewOtrMessage
          { qualifiedNewOtrSender = newOtrSender message,
            qualifiedNewOtrRecipients = qualifiedRecipients,
            qualifiedNewOtrNativePush = newOtrNativePush message,
            qualifiedNewOtrTransient = newOtrTransient message,
            qualifiedNewOtrNativePriority = newOtrNativePriority message,
            qualifiedNewOtrData = foldMap fromBase64TextLenient (newOtrData message),
            qualifiedNewOtrClientMismatchStrategy = clientMismatchStrategy
          }
  unqualify (tDomain loc) <$> f qualifiedMessage

postBotMessageUnqualified ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TinyLog r,
    Member (Input UTCTime) r
  ) =>
  BotId ->
  ConvId ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postBotMessageUnqualified sender cnv ignoreMissing reportMissing message = do
  lusr <- qualifyLocal (botUserId sender)
  lcnv <- qualifyLocal cnv
  unqualifyEndpoint
    lusr
    (runLocalInput lusr . postQualifiedOtrMessage Bot (tUntagged lusr) Nothing lcnv)
    ignoreMissing
    reportMissing
    message

postOtrBroadcastUnqualified ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS 'NonBindingTeam) r,
    Member (ErrorS 'BroadcastLimitExceeded) r,
    Member NotificationSubsystem r,
    Member ExternalAccess r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postOtrBroadcastUnqualified sender zcon =
  unqualifyEndpoint
    sender
    (postBroadcast sender (Just zcon))

postOtrMessageUnqualified ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member TeamStore r,
    Member TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  Maybe IgnoreMissing ->
  Maybe ReportMissing ->
  NewOtrMessage ->
  Sem r (PostOtrResponse ClientMismatch)
postOtrMessageUnqualified sender zcon cnv =
  let lcnv = qualifyAs sender cnv
   in unqualifyEndpoint
        sender
        (runLocalInput sender . postQualifiedOtrMessage User (tUntagged sender) (Just zcon) lcnv)

updateConversationName ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS ('ActionDenied 'ModifyConversationName)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
updateConversationName lusr zcon qcnv convRename = do
  foldQualified
    lusr
    (updateLocalConversationName lusr zcon)
    (\_ _ -> throw FederationNotImplemented)
    qcnv
    convRename

updateUnqualifiedConversationName ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS ('ActionDenied 'ModifyConversationName)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
updateUnqualifiedConversationName lusr zcon cnv rename = do
  let lcnv = qualifyAs lusr cnv
  updateLocalConversationName lusr zcon lcnv rename

updateLocalConversationName ::
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Error InvalidInput) r,
    Member (ErrorS ('ActionDenied 'ModifyConversationName)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member TeamStore r
  ) =>
  Local UserId ->
  ConnId ->
  Local ConvId ->
  ConversationRename ->
  Sem r (UpdateResult Event)
updateLocalConversationName lusr zcon lcnv rename =
  getUpdateResult . fmap lcuEvent $
    updateLocalConversation @'ConversationRenameTag lcnv (tUntagged lusr) (Just zcon) rename

memberTyping ::
  ( Member NotificationSubsystem r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member ConversationStore r,
    Member MemberStore r,
    Member FederatorAccess r
  ) =>
  Local UserId ->
  ConnId ->
  Qualified ConvId ->
  TypingStatus ->
  Sem r ()
memberTyping lusr zcon qcnv ts = do
  foldQualified
    lusr
    ( \lcnv -> do
        (conv, _) <- getConversationAndMemberWithError @'ConvNotFound (tUntagged lusr) lcnv
        void $ notifyTypingIndicator conv (tUntagged lusr) (Just zcon) ts
    )
    ( \rcnv -> do
        isMemberRemoteConv <- E.checkLocalMemberRemoteConv (tUnqualified lusr) rcnv
        unless isMemberRemoteConv $ throwS @'ConvNotFound
        let rpc =
              TypingDataUpdateRequest
                { typingStatus = ts,
                  userId = tUnqualified lusr,
                  convId = tUnqualified rcnv
                }
        res <- E.runFederated rcnv (fedClient @'Galley @"update-typing-indicator" rpc)
        case res of
          TypingDataUpdateSuccess (TypingDataUpdated {..}) -> do
            pushTypingIndicatorEvents origUserId time usersInConv (Just zcon) qcnv typingStatus
          TypingDataUpdateError _ -> pure ()
    )
    qcnv

memberTypingUnqualified ::
  ( Member NotificationSubsystem r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ConversationStore r,
    Member FederatorAccess r
  ) =>
  Local UserId ->
  ConnId ->
  ConvId ->
  TypingStatus ->
  Sem r ()
memberTypingUnqualified lusr zcon cnv ts = do
  lcnv <- qualifyLocal cnv
  memberTyping lusr zcon (tUntagged lcnv) ts

addBot ::
  forall r.
  ( Member ClientStore r,
    Member ConversationStore r,
    Member (ErrorS ('ActionDenied 'AddConversationMember)) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'InvalidOperation) r,
    Member (ErrorS 'TooManyMembers) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member MemberStore r
  ) =>
  Local UserId ->
  ConnId ->
  AddBot ->
  Sem r Event
addBot lusr zcon b = do
  c <-
    E.getConversation (b ^. addBotConv) >>= noteS @'ConvNotFound
  -- Check some preconditions on adding bots to a conversation
  (bots, users) <- regularConvChecks c
  t <- input
  E.createClient (botUserId (b ^. addBotId)) (b ^. addBotClient)
  bm <- E.createBotMember (b ^. addBotService) (b ^. addBotId) (b ^. addBotConv)
  let e =
        Event
          (tUntagged (qualifyAs lusr (b ^. addBotConv)))
          Nothing
          (tUntagged lusr)
          t
          Nothing
          ( EdMembersJoin
              ( MembersJoin
                  [ SimpleMember
                      (tUntagged (qualifyAs lusr (botUserId (botMemId bm))))
                      roleNameWireAdmin
                  ]
                  InternalAdd
              )
          )
  pushNotifications
    [ def
        { origin = Just (tUnqualified lusr),
          json = toJSONObject e,
          recipients = map localMemberToRecipient users,
          isCellsEvent = shouldPushToCells c.convMetadata e,
          conn = Just zcon
        }
    ]
  E.deliverAsync (map (,e) (bm : bots))
  pure e
  where
    regularConvChecks c = do
      let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
      unless (tUnqualified lusr `isMember` users) $ throwS @'ConvNotFound
      ensureGroupConversation c
      self <- getSelfMemberFromLocals (tUnqualified lusr) users
      -- Note that in brig from where this internal handler is called, we additionally check for conversation admin role.
      -- Remember to change this if we ever want to allow non admins to add bots.
      -- Also note, that in the future we want to allow channel admins (who are not necessarily conversation admins) to add bots.
      -- Currently bots cannot be added to channels because channels are always MLS conversations which do not support bots.
      ensureActionAllowed SAddConversationMember self
      unless (any ((== b ^. addBotId) . botMemId) bots) $ do
        let botId = qualifyAs lusr (botUserId (b ^. addBotId))
        ensureMemberLimit (Data.convProtocolTag c) (toList $ Data.convLocalMembers c) [tUntagged botId]
      pure (bots, users)

rmBot ::
  ( Member ClientStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  RemoveBot ->
  Sem r (UpdateResult Event)
rmBot lusr zcon b = do
  c <-
    E.getConversation (b ^. rmBotConv) >>= noteS @'ConvNotFound
  let (bots, users) = localBotsAndUsers (Data.convLocalMembers c)
  unless (tUnqualified lusr `isMember` Data.convLocalMembers c) $
    throwS @'ConvNotFound
  -- A bot can remove itself (which will internally be triggered when a service is deleted),
  -- otherwise we have to check for the correct permissions
  unless (botUserId (b ^. rmBotId) == tUnqualified lusr) $ do
    -- Note that in brig from where this internal handler is called, we additionally check for conversation admin role.
    -- Remember to change this if we ever want to allow non admins to remove bots.
    self <- getSelfMemberFromLocals (tUnqualified lusr) users
    -- Similarly to addBot we currently only allow admins to remove bots. Once bots support MLS we, also need to check for channel admin permissions here.
    ensureActionAllowed SRemoveConversationMember self
  let lcnv = qualifyAs lusr (Data.convId c)
  if not (any ((== b ^. rmBotId) . botMemId) bots)
    then pure Unchanged
    else do
      t <- input
      do
        let evd = EdMembersLeaveRemoved (QualifiedUserIdList [tUntagged (qualifyAs lusr (botUserId (b ^. rmBotId)))])
        let e = Event (tUntagged lcnv) Nothing (tUntagged lusr) t Nothing evd
        pushNotifications
          [ def
              { origin = Just (tUnqualified lusr),
                json = toJSONObject e,
                recipients = map localMemberToRecipient users,
                isCellsEvent = shouldPushToCells c.convMetadata e,
                conn = zcon
              }
          ]
        E.deleteMembers (Data.convId c) (UserList [botUserId (b ^. rmBotId)] [])
        E.deleteClients (botUserId (b ^. rmBotId))
        E.deliverAsync (map (,e) bots)
        pure $ Updated e

updateCellsState ::
  ( Member ConversationStore r,
    Member (ErrorS ConvNotFound) r,
    Member (ErrorS InvalidOperation) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  ConvId ->
  CellsState ->
  Sem r ()
updateCellsState cnv state = do
  when (state /= CellsDisabled) $ do
    conv <- E.getConversation cnv >>= noteS @ConvNotFound
    tid <- noteS @InvalidOperation conv.convMetadata.cnvmTeam
    feat <- getFeatureForTeam @CellsConfig tid
    noteS @InvalidOperation $ guard (feat.status == FeatureStatusEnabled)
  E.setConversationCellsState cnv state

-------------------------------------------------------------------------------
-- Helpers

ensureConvMember :: (Member (ErrorS 'ConvNotFound) r) => [LocalMember] -> UserId -> Sem r ()
ensureConvMember users usr =
  unless (usr `isMember` users) $ throwS @'ConvNotFound

getConversationCodeURI ::
  ( Member (ErrorS 'ConvAccessDenied) r,
    Member CodeStore r
  ) =>
  Maybe ZHostValue ->
  Sem r HttpsUrl
getConversationCodeURI mbZHost = do
  mbURI <- E.getConversationCodeURI mbZHost
  case mbURI of
    Just uri -> pure uri
    Nothing -> throwS @'ConvAccessDenied
