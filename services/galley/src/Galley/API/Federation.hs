{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Galley.API.Federation
  ( FederationAPI,
    federationSitemap,
    onConversationUpdated,
  )
where

import Control.Error hiding (note)
import Control.Lens
import Data.Bifunctor
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Either.Combinators
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified
import Data.Range (Range (fromRange))
import qualified Data.Set as Set
import Data.Singletons (SingI (..), demote, sing)
import Data.Tagged
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Enabled
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.Message
import Galley.API.MLS.One2One
import Galley.API.MLS.Removal
import Galley.API.MLS.SubConversation hiding (leaveSubConversation)
import Galley.API.MLS.Util
import Galley.API.MLS.Welcome
import Galley.API.Mapping
import qualified Galley.API.Mapping as Mapping
import Galley.API.Message
import Galley.API.Push
import Galley.API.Util
import Galley.App
import qualified Galley.Data.Conversation as Data
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FireAndForget as E
import qualified Galley.Effects.MemberStore as E
import Galley.Options
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.One2One
import Galley.Types.UserList (UserList (UserList))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal.Kind (Append)
import Polysemy.Resource
import Polysemy.TinyLog
import qualified Polysemy.TinyLog as P
import Servant (ServerT)
import Servant.API
import qualified System.Logger.Class as Log
import Wire.API.Conversation hiding (Member)
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Common (EmptyResponse (..))
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Message
import Wire.API.Routes.Named
import Wire.API.ServantProto
import Wire.API.User (BaseProtocolTag (..))

type FederationAPI = "federation" :> FedApi 'Galley

-- | Convert a polysemy handler to an 'API' value.
federationSitemap ::
  ServerT FederationAPI (Sem GalleyEffects)
federationSitemap =
  Named @"on-conversation-created" onConversationCreated
    :<|> Named @"get-conversations" getConversations
    :<|> Named @"on-conversation-updated" onConversationUpdated
    :<|> Named @"leave-conversation" (callsFed (exposeAnnotations leaveConversation))
    :<|> Named @"on-message-sent" onMessageSent
    :<|> Named @"send-message" (callsFed (exposeAnnotations sendMessage))
    :<|> Named @"on-user-deleted-conversations" (callsFed (exposeAnnotations onUserDeleted))
    :<|> Named @"update-conversation" (callsFed (exposeAnnotations updateConversation))
    :<|> Named @"mls-welcome" mlsSendWelcome
    :<|> Named @"on-mls-message-sent" onMLSMessageSent
    :<|> Named @"send-mls-message" (callsFed (exposeAnnotations sendMLSMessage))
    :<|> Named @"send-mls-commit-bundle" (callsFed (exposeAnnotations sendMLSCommitBundle))
    :<|> Named @"query-group-info" queryGroupInfo
    :<|> Named @"on-client-removed" (callsFed (exposeAnnotations onClientRemoved))
    :<|> Named @"update-typing-indicator" (callsFed (exposeAnnotations updateTypingIndicator))
    :<|> Named @"on-typing-indicator-updated" onTypingIndicatorUpdated
    :<|> Named @"get-sub-conversation" getSubConversationForRemoteUser
    :<|> Named @"delete-sub-conversation" (callsFed deleteSubConversationForRemoteUser)
    :<|> Named @"leave-sub-conversation" (callsFed leaveSubConversation)
    :<|> Named @"get-one2one-conversation" getOne2OneConversation

onClientRemoved ::
  ( Member ConversationStore r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Domain ->
  ClientRemovedRequest ->
  Sem r EmptyResponse
onClientRemoved domain req = do
  let qusr = Qualified (F.crrUser req) domain
  whenM isMLSEnabled $ do
    for_ (F.crrConvs req) $ \convId -> do
      mConv <- E.getConversation convId
      for mConv $ \conv -> do
        lconv <- qualifyLocal conv
        removeClient lconv qusr (F.crrClient req)
  pure EmptyResponse

onConversationCreated ::
  ( Member BrigAccess r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (Input (Local ())) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Domain ->
  F.ConversationCreated ConvId ->
  Sem r ()
onConversationCreated domain rc = do
  let qrc = fmap (toRemoteUnsafe domain) rc
  loc <- qualifyLocal ()
  let (localUserIds, _) = partitionQualified loc (map omQualifiedId (toList (F.ccNonCreatorMembers rc)))

  addedUserIds <-
    addLocalUsersToRemoteConv
      (F.ccCnvId qrc)
      (tUntagged (F.ccRemoteOrigUserId qrc))
      localUserIds

  let connectedMembers =
        Set.filter
          ( foldQualified
              loc
              (flip Set.member addedUserIds . tUnqualified)
              (const True)
              . omQualifiedId
          )
          (F.ccNonCreatorMembers rc)
  -- Make sure to notify only about local users connected to the adder
  let qrcConnected = qrc {F.ccNonCreatorMembers = connectedMembers}

  for_ (fromConversationCreated loc qrcConnected) $ \(mem, c) -> do
    let event =
          Event
            (tUntagged (F.ccCnvId qrcConnected))
            Nothing
            (tUntagged (F.ccRemoteOrigUserId qrcConnected))
            (F.ccTime qrcConnected)
            (EdConversation c)
    pushConversationEvent Nothing event (qualifyAs loc [qUnqualified . Public.memId $ mem]) []

getConversations ::
  ( Member ConversationStore r,
    Member (Input (Local ())) r
  ) =>
  Domain ->
  F.GetConversationsRequest ->
  Sem r F.GetConversationsResponse
getConversations domain (F.GetConversationsRequest uid cids) = do
  let ruid = toRemoteUnsafe domain uid
  loc <- qualifyLocal ()
  F.GetConversationsResponse
    . mapMaybe (Mapping.conversationToRemote (tDomain loc) ruid)
    <$> E.getConversations cids

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
onConversationUpdated ::
  ( Member BrigAccess r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member (Input (Local ())) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Domain ->
  F.ConversationUpdate ->
  Sem r ()
onConversationUpdated requestingDomain cu = do
  let rcu = toRemoteUnsafe requestingDomain cu
  void $ updateLocalStateOfRemoteConv rcu Nothing

-- as of now this will not generate the necessary events on the leaver's domain
leaveConversation ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Domain ->
  F.LeaveConversationRequest ->
  Sem r F.LeaveConversationResponse
leaveConversation requestingDomain lc = do
  let leaver = Qualified (F.lcLeaver lc) requestingDomain
  lcnv <- qualifyLocal (F.lcConvId lc)

  res <-
    runError
      . mapToRuntimeError @'ConvNotFound F.RemoveFromConversationErrorNotFound
      . mapToRuntimeError @('ActionDenied 'LeaveConversation) F.RemoveFromConversationErrorRemovalNotAllowed
      . mapToRuntimeError @'InvalidOperation F.RemoveFromConversationErrorRemovalNotAllowed
      . mapError @NoChanges (const F.RemoveFromConversationErrorUnchanged)
      $ do
        (conv, _self) <- getConversationAndMemberWithError @'ConvNotFound leaver lcnv
        outcome <-
          runError @FederationError $
            first lcuUpdate
              <$> updateLocalConversation
                @'ConversationLeaveTag
                lcnv
                leaver
                Nothing
                ()
        case outcome of
          Left e -> do
            logFederationError lcnv e
            throw . internalErr $ e
          Right update -> pure (update, conv)

  case res of
    Left e -> pure $ F.LeaveConversationResponse (Left e)
    Right ((_update, updateFailedToProcess), conv) -> do
      let remotes = filter ((== qDomain leaver) . tDomain) (rmId <$> Data.convRemoteMembers conv)
      let botsAndMembers = BotsAndMembers mempty (Set.fromList remotes) mempty
      (_, notifyFailedToProcess) <- do
        outcome <-
          runError @FederationError $
            notifyConversationAction
              SConversationLeaveTag
              leaver
              False
              Nothing
              (qualifyAs lcnv conv)
              botsAndMembers
              ()
        case outcome of
          Left e -> do
            logFederationError lcnv e
            throw . internalErr $ e
          Right v -> pure v

      pure . F.LeaveConversationResponse . Right $
        updateFailedToProcess <> notifyFailedToProcess
  where
    internalErr = InternalErrorWithDescription . LT.pack . displayException

-- FUTUREWORK: report errors to the originating backend
-- FUTUREWORK: error handling for missing / mismatched clients
-- FUTUREWORK: support bots
onMessageSent ::
  ( Member GundeckAccess r,
    Member ExternalAccess r,
    Member MemberStore r,
    Member (Input (Local ())) r,
    Member P.TinyLog r
  ) =>
  Domain ->
  F.RemoteMessage ConvId ->
  Sem r ()
onMessageSent domain rmUnqualified = do
  let rm = fmap (toRemoteUnsafe domain) rmUnqualified
      convId = tUntagged $ F.rmConversation rm
      msgMetadata =
        MessageMetadata
          { mmNativePush = F.rmPush rm,
            mmTransient = F.rmTransient rm,
            mmNativePriority = F.rmPriority rm,
            mmData = F.rmData rm
          }
      recipientMap = userClientMap $ F.rmRecipients rm
      msgs = toMapOf (itraversed <.> itraversed) recipientMap
  (members, allMembers) <-
    first Set.fromList
      <$> E.selectRemoteMembers (Map.keys recipientMap) (F.rmConversation rm)
  unless allMembers $
    P.warn $
      Log.field "conversation" (toByteString' (qUnqualified convId))
        Log.~~ Log.field "domain" (toByteString' (qDomain convId))
        Log.~~ Log.msg
          ( "Attempt to send remote message to local\
            \ users not in the conversation" ::
              ByteString
          )
  loc <- qualifyLocal ()
  void $
    sendLocalMessages
      loc
      (F.rmTime rm)
      (F.rmSender rm)
      (F.rmSenderClient rm)
      Nothing
      (Just convId)
      mempty
      msgMetadata
      (Map.filterWithKey (\(uid, _) _ -> Set.member uid members) msgs)

sendMessage ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member ConversationStore r,
    Member (Error InvalidInput) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member ExternalAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Domain ->
  F.ProteusMessageSendRequest ->
  Sem r F.MessageSendResponse
sendMessage originDomain msr = do
  let sender = Qualified (F.pmsrSender msr) originDomain
  msg <- either throwErr pure (fromProto (fromBase64ByteString (F.pmsrRawMessage msr)))
  lcnv <- qualifyLocal (F.pmsrConvId msr)
  F.MessageSendResponse <$> postQualifiedOtrMessage User sender Nothing lcnv msg
  where
    throwErr = throw . InvalidPayload . LT.pack

onUserDeleted ::
  ( Member ConversationStore r,
    Member FederatorAccess r,
    Member FireAndForget r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (Input Env) r,
    Member MemberStore r,
    Member ProposalStore r,
    Member SubConversationStore r,
    Member TinyLog r
  ) =>
  Domain ->
  F.UserDeletedConversationsNotification ->
  Sem r EmptyResponse
onUserDeleted origDomain udcn = do
  let deletedUser = toRemoteUnsafe origDomain (F.udcvUser udcn)
      untaggedDeletedUser = tUntagged deletedUser
      convIds = F.udcvConversations udcn

  E.spawnMany $
    fromRange convIds <&> \c -> do
      lc <- qualifyLocal c
      mconv <- E.getConversation c
      E.deleteMembers c (UserList [] [deletedUser])
      for_ mconv $ \conv -> do
        when (isRemoteMember deletedUser (Data.convRemoteMembers conv)) $
          case Data.convType conv of
            -- No need for a notification on One2One conv as the user is being
            -- deleted and that notification should suffice.
            Public.One2OneConv -> pure ()
            -- No need for a notification on Connect Conv as there should be no
            -- other user in the conv.
            Public.ConnectConv -> pure ()
            -- The self conv cannot be on a remote backend.
            Public.SelfConv -> pure ()
            Public.RegularConv -> do
              let botsAndMembers = convBotsAndMembers conv
              removeUser (qualifyAs lc conv) (tUntagged deletedUser)
              outcome <-
                runError @FederationError $
                  notifyConversationAction
                    (sing @'ConversationLeaveTag)
                    untaggedDeletedUser
                    False
                    Nothing
                    (qualifyAs lc conv)
                    botsAndMembers
                    ()
              whenLeft outcome . logFederationError $ lc
  pure EmptyResponse

updateConversation ::
  forall r.
  ( Member BrigAccess r,
    Member CodeStore r,
    Member BotAccess r,
    Member FireAndForget r,
    Member (Error FederationError) r,
    Member (Error InvalidInput) r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member (Error InternalError) r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member ProposalStore r,
    Member TeamStore r,
    Member TinyLog r,
    Member ConversationStore r,
    Member SubConversationStore r,
    Member TeamFeatureStore r,
    Member (Input (Local ())) r
  ) =>
  Domain ->
  F.ConversationUpdateRequest ->
  Sem r ConversationUpdateResponse
updateConversation origDomain updateRequest = do
  loc <- qualifyLocal ()
  let rusr = toRemoteUnsafe origDomain (F.curUser updateRequest)
      lcnv = qualifyAs loc (F.curConvId updateRequest)

  mkResponse $ case F.curAction updateRequest of
    SomeConversationAction tag action -> case tag of
      SConversationJoinTag ->
        mapToGalleyError @(HasConversationActionGalleyErrors 'ConversationJoinTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationJoinTag lcnv (tUntagged rusr) Nothing action
      SConversationLeaveTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationLeaveTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationLeaveTag lcnv (tUntagged rusr) Nothing action
      SConversationRemoveMembersTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationRemoveMembersTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationRemoveMembersTag lcnv (tUntagged rusr) Nothing action
      SConversationMemberUpdateTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationMemberUpdateTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationMemberUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationDeleteTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationDeleteTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationDeleteTag lcnv (tUntagged rusr) Nothing action
      SConversationRenameTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationRenameTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationRenameTag lcnv (tUntagged rusr) Nothing action
      SConversationMessageTimerUpdateTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationMessageTimerUpdateTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationMessageTimerUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationReceiptModeUpdateTag ->
        mapToGalleyError @(HasConversationActionGalleyErrors 'ConversationReceiptModeUpdateTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationReceiptModeUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationAccessDataTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationAccessDataTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationAccessDataTag lcnv (tUntagged rusr) Nothing action
      SConversationUpdateProtocolTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationUpdateProtocolTag)
          . fmap (first lcuUpdate)
          $ updateLocalConversation @'ConversationUpdateProtocolTag lcnv (tUntagged rusr) Nothing action
  where
    mkResponse = fmap toResponse . runError @GalleyError . runError @NoChanges

    toResponse (Left galleyErr) = F.ConversationUpdateResponseError galleyErr
    toResponse (Right (Left NoChanges)) = F.ConversationUpdateResponseNoChanges
    toResponse (Right (Right (update, ftp))) = F.ConversationUpdateResponseUpdate update ftp

sendMLSCommitBundle ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member Resource r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member SubConversationStore r,
    Member ProposalStore r
  ) =>
  Domain ->
  F.MLSMessageSendRequest ->
  Sem r F.MLSMessageResponse
sendMLSCommitBundle remoteDomain msr =
  fmap (either (F.MLSMessageResponseProtocolError . unTagged) id)
    . runError @MLSProtocolError
    . fmap (either F.MLSMessageResponseError id)
    . runError
    . fmap (either (F.MLSMessageResponseProposalFailure . pfInner) id)
    . runError
    . mapToGalleyError @MLSBundleStaticErrors
    $ do
      assertMLSEnabled
      loc <- qualifyLocal ()
      let sender = toRemoteUnsafe remoteDomain (F.mmsrSender msr)
      bundle <-
        either (throw . mlsProtocolError) pure $
          decodeMLS' (fromBase64ByteString (F.mmsrRawMessage msr))

      ibundle <- noteS @'MLSUnsupportedMessage $ mkIncomingBundle bundle
      qConvOrSub <- getConvFromGroupId ibundle.groupId
      when (qUnqualified qConvOrSub /= F.mmsrConvOrSubId msr) $ throwS @'MLSGroupConversationMismatch
      uncurry F.MLSMessageResponseUpdates . (,mempty) . map lcuUpdate
        <$> postMLSCommitBundle
          loc
          (tUntagged sender)
          (mmsrSenderClient msr)
          qConvOrSub
          Nothing
          ibundle

sendMLSMessage ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member (Input Opts) r,
    Member (Input UTCTime) r,
    Member LegalHoldStore r,
    Member MemberStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member ProposalStore r,
    Member SubConversationStore r
  ) =>
  Domain ->
  F.MLSMessageSendRequest ->
  Sem r F.MLSMessageResponse
sendMLSMessage remoteDomain msr =
  fmap (either (F.MLSMessageResponseProtocolError . unTagged) id)
    . runError @MLSProtocolError
    . fmap (either F.MLSMessageResponseError id)
    . runError
    . fmap (either (F.MLSMessageResponseProposalFailure . pfInner) id)
    . runError
    . mapToGalleyError @MLSMessageStaticErrors
    $ do
      assertMLSEnabled
      loc <- qualifyLocal ()
      let sender = toRemoteUnsafe remoteDomain (F.mmsrSender msr)
      raw <- either (throw . mlsProtocolError) pure $ decodeMLS' (fromBase64ByteString (F.mmsrRawMessage msr))
      msg <- noteS @'MLSUnsupportedMessage $ mkIncomingMessage raw
      qConvOrSub <- getConvFromGroupId msg.groupId
      when (qUnqualified qConvOrSub /= F.mmsrConvOrSubId msr) $ throwS @'MLSGroupConversationMismatch
      uncurry F.MLSMessageResponseUpdates . first (map lcuUpdate)
        <$> postMLSMessage
          loc
          (tUntagged sender)
          (mmsrSenderClient msr)
          qConvOrSub
          Nothing
          msg

mlsSendWelcome ::
  ( Member (Error InternalError) r,
    Member GundeckAccess r,
    Member ExternalAccess r,
    Member P.TinyLog r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r
  ) =>
  Domain ->
  F.MLSWelcomeRequest ->
  Sem r F.MLSWelcomeResponse
mlsSendWelcome origDomain req = do
  fmap (either (const MLSWelcomeMLSNotEnabled) (const MLSWelcomeSent))
    . runError @(Tagged 'MLSNotEnabled ())
    $ do
      assertMLSEnabled
      loc <- qualifyLocal ()
      now <- input
      welcome <-
        either (throw . InternalErrorWithDescription . LT.fromStrict) pure $
          decodeMLS' (fromBase64ByteString req.welcomeMessage)
      sendLocalWelcomes req.qualifiedConvId (Qualified req.originatingUser origDomain) Nothing now welcome (qualifyAs loc req.recipients)

onMLSMessageSent ::
  ( Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Domain ->
  F.RemoteMLSMessage ->
  Sem r F.RemoteMLSMessageResponse
onMLSMessageSent domain rmm =
  fmap (either (const RemoteMLSMessageMLSNotEnabled) (const RemoteMLSMessageOk))
    . runError @(Tagged 'MLSNotEnabled ())
    $ do
      assertMLSEnabled
      loc <- qualifyLocal ()
      let rcnv = toRemoteUnsafe domain (F.rmmConversation rmm)
      let users = Set.fromList (map fst (F.rmmRecipients rmm))
      (members, allMembers) <-
        first Set.fromList
          <$> E.selectRemoteMembers (toList users) rcnv
      unless allMembers $
        P.warn $
          Log.field "conversation" (toByteString' (tUnqualified rcnv))
            Log.~~ Log.field "domain" (toByteString' (tDomain rcnv))
            Log.~~ Log.msg
              ( "Attempt to send remote message to local\
                \ users not in the conversation" ::
                  ByteString
              )
      let recipients = filter (\(u, _) -> Set.member u members) (F.rmmRecipients rmm)
      -- FUTUREWORK: support local bots
      let e =
            Event (tUntagged rcnv) (F.rmmSubConversation rmm) (F.rmmSender rmm) (F.rmmTime rmm) $
              EdMLSMessage (fromBase64ByteString (F.rmmMessage rmm))

      -- FUTUREWORK: Send only 1 push, after broken Eq, Ord instances of Recipient is fixed. Find other place via tag [FTRPUSHORD]
      for_ recipients $ \(u, c) -> do
        runMessagePush loc (Just (tUntagged rcnv)) $
          newMessagePush mempty Nothing (F.rmmMetadata rmm) [(u, c)] e

queryGroupInfo ::
  ( Member ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
    Member SubConversationStore r,
    Member MemberStore r
  ) =>
  Domain ->
  F.GetGroupInfoRequest ->
  Sem r F.GetGroupInfoResponse
queryGroupInfo origDomain req =
  fmap (either F.GetGroupInfoResponseError F.GetGroupInfoResponseState)
    . runError @GalleyError
    . mapToGalleyError @MLSGroupInfoStaticErrors
    $ do
      assertMLSEnabled
      let sender = toRemoteUnsafe origDomain . ggireqSender $ req
      state <- case ggireqConv req of
        Conv convId -> do
          lconvId <- qualifyLocal convId
          getGroupInfoFromLocalConv (tUntagged sender) lconvId
        SubConv convId subConvId -> do
          lconvId <- qualifyLocal convId
          getSubConversationGroupInfoFromLocalConv (tUntagged sender) subConvId lconvId
      pure
        . Base64ByteString
        . unGroupInfoData
        $ state

updateTypingIndicator ::
  ( Member GundeckAccess r,
    Member FederatorAccess r,
    Member ConversationStore r,
    Member (Input UTCTime) r,
    Member (Input (Local ())) r
  ) =>
  Domain ->
  F.TypingDataUpdateRequest ->
  Sem r F.TypingDataUpdateResponse
updateTypingIndicator origDomain TypingDataUpdateRequest {..} = do
  let qusr = Qualified tdurUserId origDomain
  lcnv <- qualifyLocal tdurConvId

  ret <- runError
    . mapToRuntimeError @'ConvNotFound ConvNotFound
    $ do
      (conv, _) <- getConversationAndMemberWithError @'ConvNotFound qusr lcnv
      notifyTypingIndicator conv qusr Nothing tdurTypingStatus

  pure (either TypingDataUpdateError TypingDataUpdateSuccess ret)

onTypingIndicatorUpdated ::
  ( Member GundeckAccess r
  ) =>
  Domain ->
  TypingDataUpdated ->
  Sem r EmptyResponse
onTypingIndicatorUpdated origDomain TypingDataUpdated {..} = do
  let qcnv = Qualified tudConvId origDomain
  pushTypingIndicatorEvents tudOrigUserId tudTime tudUsersInConv Nothing qcnv tudTypingStatus
  pure EmptyResponse

getSubConversationForRemoteUser ::
  Members
    '[ SubConversationStore,
       ConversationStore,
       Input (Local ()),
       Error InternalError,
       P.TinyLog
     ]
    r =>
  Domain ->
  GetSubConversationsRequest ->
  Sem r GetSubConversationsResponse
getSubConversationForRemoteUser domain GetSubConversationsRequest {..} =
  fmap (either F.GetSubConversationsResponseError F.GetSubConversationsResponseSuccess)
    . runError @GalleyError
    . mapToGalleyError @MLSGetSubConvStaticErrors
    $ do
      let qusr = Qualified gsreqUser domain
      lconv <- qualifyLocal gsreqConv
      getLocalSubConversation qusr lconv gsreqSubConv

leaveSubConversation ::
  ( HasLeaveSubConversationEffects r,
    Members
      '[ Input (Local ()),
         Resource
       ]
      r
  ) =>
  Domain ->
  LeaveSubConversationRequest ->
  Sem r LeaveSubConversationResponse
leaveSubConversation domain lscr = do
  let rusr = toRemoteUnsafe domain (lscrUser lscr)
      cid = mkClientIdentity (tUntagged rusr) (lscrClient lscr)
  lcnv <- qualifyLocal (lscrConv lscr)
  fmap (either (LeaveSubConversationResponseProtocolError . unTagged) id)
    . runError @MLSProtocolError
    . fmap (either LeaveSubConversationResponseError id)
    . runError @GalleyError
    . mapToGalleyError @LeaveSubConversationStaticErrors
    $ leaveLocalSubConversation cid lcnv (lscrSubConv lscr)
      $> LeaveSubConversationResponseOk

deleteSubConversationForRemoteUser ::
  ( Members
      '[ ConversationStore,
         FederatorAccess,
         Input (Local ()),
         Input Env,
         MemberStore,
         Resource,
         SubConversationStore
       ]
      r
  ) =>
  Domain ->
  DeleteSubConversationFedRequest ->
  Sem r DeleteSubConversationResponse
deleteSubConversationForRemoteUser domain DeleteSubConversationFedRequest {..} =
  fmap
    ( either
        F.DeleteSubConversationResponseError
        (\() -> F.DeleteSubConversationResponseSuccess)
    )
    . runError @GalleyError
    . mapToGalleyError @MLSDeleteSubConvStaticErrors
    $ do
      let qusr = Qualified dscreqUser domain
          dsc = DeleteSubConversationRequest dscreqGroupId dscreqEpoch
      lconv <- qualifyLocal dscreqConv
      deleteLocalSubConversation qusr lconv dscreqSubConv dsc

getOne2OneConversation ::
  ( Member ConversationStore r,
    Member (Input (Local ())) r,
    Member (Error InternalError) r,
    Member BrigAccess r
  ) =>
  Domain ->
  GetOne2OneConversationRequest ->
  Sem r GetOne2OneConversationResponse
getOne2OneConversation domain (GetOne2OneConversationRequest self other) =
  fmap (Imports.fromRight GetOne2OneConversationNotConnected)
    . runError @(Tagged 'NotConnected ())
    $ do
      lother <- qualifyLocal other
      let rself = toRemoteUnsafe domain self
      ensureConnectedToRemotes lother [rself]
      let getLocal lconv = do
            mconv <- E.getConversation (tUnqualified lconv)
            fmap GetOne2OneConversationOk $ case mconv of
              Nothing -> pure (localMLSOne2OneConversationAsRemote lother lconv)
              Just conv ->
                note
                  (InternalErrorWithDescription "Unexpected member list in 1-1 conversation")
                  (conversationToRemote (tDomain lother) rself conv)
      foldQualified
        lother
        getLocal
        (const (pure GetOne2OneConversationBackendMismatch))
        (one2OneConvId BaseProtocolMLSTag (tUntagged lother) (tUntagged rself))

--------------------------------------------------------------------------------
-- Error handling machinery

class ToGalleyRuntimeError (effs :: EffectRow) r where
  mapToGalleyError ::
    Member (Error GalleyError) r =>
    Sem (Append effs r) a ->
    Sem r a

instance ToGalleyRuntimeError '[] r where
  mapToGalleyError = id

instance
  forall (err :: GalleyError) effs r.
  ( ToGalleyRuntimeError effs r,
    SingI err,
    Member (Error GalleyError) (Append effs r)
  ) =>
  ToGalleyRuntimeError (ErrorS err ': effs) r
  where
  mapToGalleyError act =
    mapToGalleyError @effs @r $
      runError act >>= \case
        Left _ -> throw (demote @err)
        Right res -> pure res

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Log a federation error that is impossible in processing a remote request
-- for a local conversation.
logFederationError ::
  Member P.TinyLog r =>
  Local ConvId ->
  FederationError ->
  Sem r ()
logFederationError lc e =
  P.warn $
    Log.field "conversation" (toByteString' (tUnqualified lc))
      Log.~~ Log.field "domain" (toByteString' (tDomain lc))
      Log.~~ Log.msg
        ( "An impossible federation error occurred when deleting\
          \ a user from a local conversation: "
            <> displayException e
        )
