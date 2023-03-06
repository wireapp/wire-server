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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Galley.API.Federation where

import Control.Error
import Control.Lens (itraversed, preview, to, (<.>))
import Data.Bifunctor
import Data.ByteString.Conversion (toByteString')
import Data.Containers.ListUtils (nubOrd)
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty (..))
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
import Galley.API.MLS.KeyPackage
import Galley.API.MLS.Message
import Galley.API.MLS.Removal
import Galley.API.MLS.Welcome
import qualified Galley.API.Mapping as Mapping
import Galley.API.Message
import Galley.API.Push
import Galley.API.Util
import Galley.App
import qualified Galley.Data.Conversation as Data
import Galley.Effects
import qualified Galley.Effects.BrigAccess as E
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FireAndForget as E
import qualified Galley.Effects.MemberStore as E
import Galley.Effects.ProposalStore (ProposalStore)
import Galley.Options
import Galley.Types.Conversations.Members
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
import Wire.API.Connection
import Wire.API.Conversation hiding (Member)
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Common (EmptyResponse (..))
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Federation.Error
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.Message
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Welcome
import Wire.API.Message
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import Wire.API.ServantProto

type FederationAPI = "federation" :> FedApi 'Galley

-- | Convert a polysemy handler to an 'API' value.
federationSitemap ::
  ServerT FederationAPI (Sem GalleyEffects)
federationSitemap =
  Named @"on-conversation-created" onConversationCreated
    :<|> Named @"on-new-remote-conversation" onNewRemoteConversation
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

onNewRemoteConversation ::
  Member ConversationStore r =>
  Domain ->
  F.NewRemoteConversation ->
  Sem r EmptyResponse
onNewRemoteConversation domain nrc = do
  -- update group_id -> conv_id mapping
  for_ (preview (to F.nrcProtocol . _ProtocolMLS) nrc) $ \mls ->
    E.setGroupId (cnvmlsGroupId mls) (Qualified (F.nrcConvId nrc) domain)

  pure EmptyResponse

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

getLocalUsers :: Domain -> NonEmpty (Qualified UserId) -> [UserId]
getLocalUsers localDomain = map qUnqualified . filter ((== localDomain) . qDomain) . toList

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
  loc <- qualifyLocal ()
  let rconvId = toRemoteUnsafe requestingDomain (F.cuConvId cu)
      qconvId = tUntagged rconvId

  -- Note: we generally do not send notifications to users that are not part of
  -- the conversation (from our point of view), to prevent spam from the remote
  -- backend. See also the comment below.
  (presentUsers, allUsersArePresent) <-
    E.selectRemoteMembers (F.cuAlreadyPresentUsers cu) rconvId

  -- Perform action, and determine extra notification targets.
  --
  -- When new users are being added to the conversation, we consider them as
  -- notification targets. Since we check connections before letting
  -- people being added, this is safe against spam. However, if users that
  -- are not in the conversations are being removed or have their membership state
  -- updated, we do **not** add them to the list of targets, because we have no
  -- way to make sure that they are actually supposed to receive that notification.

  (mActualAction :: Maybe SomeConversationAction, extraTargets :: [UserId]) <- case F.cuAction cu of
    sca@(SomeConversationAction singTag action) -> case singTag of
      SConversationJoinTag -> do
        let ConversationJoin toAdd role = action
        let (localUsers, remoteUsers) = partitionQualified loc toAdd
        addedLocalUsers <- Set.toList <$> addLocalUsersToRemoteConv rconvId (F.cuOrigUserId cu) localUsers
        let allAddedUsers = map (tUntagged . qualifyAs loc) addedLocalUsers <> map tUntagged remoteUsers
        case allAddedUsers of
          [] -> pure (Nothing, []) -- If no users get added, its like no action was performed.
          (u : us) -> pure (Just (SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (u :| us) role)), addedLocalUsers)
      SConversationLeaveTag -> do
        let users = foldQualified loc (pure . tUnqualified) (const []) (F.cuOrigUserId cu)
        E.deleteMembersInRemoteConversation rconvId users
        pure (Just sca, [])
      SConversationRemoveMembersTag -> do
        let localUsers = getLocalUsers (tDomain loc) action
        E.deleteMembersInRemoteConversation rconvId localUsers
        pure (Just sca, [])
      SConversationMemberUpdateTag ->
        pure (Just sca, [])
      SConversationDeleteTag -> do
        E.deleteMembersInRemoteConversation rconvId presentUsers
        pure (Just sca, [])
      SConversationRenameTag -> pure (Just sca, [])
      SConversationMessageTimerUpdateTag -> pure (Just sca, [])
      SConversationReceiptModeUpdateTag -> pure (Just sca, [])
      SConversationAccessDataTag -> pure (Just sca, [])

  unless allUsersArePresent $
    P.warn $
      Log.field "conversation" (toByteString' (F.cuConvId cu))
        . Log.field "domain" (toByteString' requestingDomain)
        . Log.msg
          ( "Attempt to send notification about conversation update \
            \to users not in the conversation" ::
              ByteString
          )

  -- Send notifications
  for_ mActualAction $ \(SomeConversationAction tag action) -> do
    let event = conversationActionToEvent tag (F.cuTime cu) (F.cuOrigUserId cu) qconvId Nothing action
        targets = nubOrd $ presentUsers <> extraTargets
    -- FUTUREWORK: support bots?
    pushConversationEvent Nothing event (qualifyAs loc targets) []

addLocalUsersToRemoteConv ::
  ( Member BrigAccess r,
    Member MemberStore r,
    Member P.TinyLog r
  ) =>
  Remote ConvId ->
  Qualified UserId ->
  [UserId] ->
  Sem r (Set UserId)
addLocalUsersToRemoteConv remoteConvId qAdder localUsers = do
  connStatus <- E.getConnections localUsers (Just [qAdder]) (Just Accepted)
  let localUserIdsSet = Set.fromList localUsers
      connected = Set.fromList $ fmap csv2From connStatus
      unconnected = Set.difference localUserIdsSet connected
      connectedList = Set.toList connected

  -- FUTUREWORK: Consider handling the discrepancy between the views of the
  -- conversation-owning backend and the local backend
  unless (Set.null unconnected) $
    P.warn $
      Log.msg ("A remote user is trying to add unconnected local users to a remote conversation" :: Text)
        . Log.field "remote_user" (show qAdder)
        . Log.field "local_unconnected_users" (show unconnected)

  -- Update the local view of the remote conversation by adding only those local
  -- users that are connected to the adder
  E.createMembersInRemoteConversation remoteConvId connectedList
  pure connected

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
    Member TinyLog r
  ) =>
  Domain ->
  F.LeaveConversationRequest ->
  Sem r F.LeaveConversationResponse
leaveConversation requestingDomain lc = do
  let leaver :: Remote UserId = qTagUnsafe $ Qualified (F.lcLeaver lc) requestingDomain
  lcnv <- qualifyLocal (F.lcConvId lc)

  res <-
    runError
      . mapToRuntimeError @'ConvNotFound F.RemoveFromConversationErrorNotFound
      . mapToRuntimeError @('ActionDenied 'LeaveConversation) F.RemoveFromConversationErrorRemovalNotAllowed
      . mapToRuntimeError @'InvalidOperation F.RemoveFromConversationErrorRemovalNotAllowed
      . mapError @NoChanges (const F.RemoveFromConversationErrorUnchanged)
      $ do
        (conv, _self) <- getConversationAndMemberWithError @'ConvNotFound (tUntagged leaver) lcnv
        update <-
          lcuUpdate
            <$> updateLocalConversation
              @'ConversationLeaveTag
              lcnv
              (tUntagged leaver)
              Nothing
              ()
        pure (update, conv)

  case res of
    Left e -> pure $ F.LeaveConversationResponse (Left e)
    Right (_update, conv) -> do
      let remotes = filter ((== tDomain leaver) . tDomain) (rmId <$> Data.convRemoteMembers conv)
      let botsAndMembers = BotsAndMembers mempty (Set.fromList remotes) mempty
      _ <-
        notifyConversationAction
          False
          SConversationLeaveTag
          (tUntagged leaver)
          False
          Nothing
          (qualifyAs lcnv conv)
          botsAndMembers
          ()

      pure $ F.LeaveConversationResponse (Right ())

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
    sendLocalMessages @'NormalMessage
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
              void $
                notifyConversationAction
                  False
                  (sing @'ConversationLeaveTag)
                  untaggedDeletedUser
                  False
                  Nothing
                  (qualifyAs lc conv)
                  botsAndMembers
                  ()
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
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationJoinTag lcnv (tUntagged rusr) Nothing action
      SConversationLeaveTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationLeaveTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationLeaveTag lcnv (tUntagged rusr) Nothing action
      SConversationRemoveMembersTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationRemoveMembersTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationRemoveMembersTag lcnv (tUntagged rusr) Nothing action
      SConversationMemberUpdateTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationMemberUpdateTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationMemberUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationDeleteTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationDeleteTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationDeleteTag lcnv (tUntagged rusr) Nothing action
      SConversationRenameTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationRenameTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationRenameTag lcnv (tUntagged rusr) Nothing action
      SConversationMessageTimerUpdateTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationMessageTimerUpdateTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationMessageTimerUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationReceiptModeUpdateTag ->
        mapToGalleyError @(HasConversationActionGalleyErrors 'ConversationReceiptModeUpdateTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationReceiptModeUpdateTag lcnv (tUntagged rusr) Nothing action
      SConversationAccessDataTag ->
        mapToGalleyError
          @(HasConversationActionGalleyErrors 'ConversationAccessDataTag)
          . fmap lcuUpdate
          $ updateLocalConversation @'ConversationAccessDataTag lcnv (tUntagged rusr) Nothing action
  where
    mkResponse = fmap toResponse . runError @GalleyError . runError @NoChanges

    toResponse (Left galleyErr) = F.ConversationUpdateResponseError galleyErr
    toResponse (Right (Left NoChanges)) = F.ConversationUpdateResponseNoChanges
    toResponse (Right (Right update)) = F.ConversationUpdateResponseUpdate update

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
      bundle <- either (throw . mlsProtocolError) pure $ deserializeCommitBundle (fromBase64ByteString (F.mmsrRawMessage msr))
      let msg = rmValue (cbCommitMsg bundle)
      qcnv <- E.getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
      when (Conv (qUnqualified qcnv) /= F.mmsrConvOrSubId msr) $ throwS @'MLSGroupConversationMismatch
      F.MLSMessageResponseUpdates . map lcuUpdate
        <$> postMLSCommitBundle loc (tUntagged sender) Nothing qcnv Nothing bundle

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
    Member Resource r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member ProposalStore r
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
      case rmValue raw of
        SomeMessage _ msg -> do
          qcnv <- E.getConversationIdByGroupId (msgGroupId msg) >>= noteS @'ConvNotFound
          when (Conv (qUnqualified qcnv) /= F.mmsrConvOrSubId msr) $ throwS @'MLSGroupConversationMismatch
          F.MLSMessageResponseUpdates . map lcuUpdate
            <$> postMLSMessage loc (tUntagged sender) Nothing qcnv Nothing raw

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

mlsSendWelcome ::
  ( Member BrigAccess r,
    Member (Error InternalError) r,
    Member GundeckAccess r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r
  ) =>
  Domain ->
  F.MLSWelcomeRequest ->
  Sem r F.MLSWelcomeResponse
mlsSendWelcome _origDomain (fromBase64ByteString . F.unMLSWelcomeRequest -> rawWelcome) =
  fmap (either (const MLSWelcomeMLSNotEnabled) (const MLSWelcomeSent))
    . runError @(Tagged 'MLSNotEnabled ())
    $ do
      assertMLSEnabled
      loc <- qualifyLocal ()
      now <- input
      welcome <- either (throw . InternalErrorWithDescription . LT.fromStrict) pure $ decodeMLS' rawWelcome
      -- Extract only recipients local to this backend
      rcpts <-
        fmap catMaybes
          $ traverse
            ( fmap (fmap cidQualifiedClient . hush)
                . runError @(Tagged 'MLSKeyPackageRefNotFound ())
                . derefKeyPackage
                . gsNewMember
            )
          $ welSecrets welcome
      let lrcpts = qualifyAs loc $ fst $ partitionQualified loc rcpts
      sendLocalWelcomes Nothing now rawWelcome lrcpts

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
            Event (tUntagged rcnv) Nothing (F.rmmSender rmm) (F.rmmTime rmm) $
              EdMLSMessage (fromBase64ByteString (F.rmmMessage rmm))
      let mkPush :: (UserId, ClientId) -> MessagePush 'NormalMessage
          mkPush uc = newMessagePush loc mempty Nothing (F.rmmMetadata rmm) uc e

      runMessagePush loc (Just (tUntagged rcnv)) $
        foldMap mkPush recipients

queryGroupInfo ::
  ( Member ConversationStore r,
    Member (Input (Local ())) r,
    Member (Input Env) r,
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
      lconvId <- qualifyLocal . ggireqConv $ req
      let sender = toRemoteUnsafe origDomain . ggireqSender $ req
      state <- getGroupInfoFromLocalConv (tUntagged sender) lconvId
      pure
        . Base64ByteString
        . unOpaquePublicGroupState
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
