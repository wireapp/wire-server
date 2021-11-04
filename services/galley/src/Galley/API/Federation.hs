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
module Galley.API.Federation where

import Brig.Types.Connection (Relation (Accepted))
import Control.Lens (itraversed, (<.>))
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Conversion (toByteString')
import Data.Containers.ListUtils (nubOrd)
import Data.Domain
import Data.Id (ConvId, UserId)
import Data.Json.Util (Base64ByteString (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import Galley.API.Error (invalidPayload)
import qualified Galley.API.Mapping as Mapping
import Galley.API.Message (MessageMetadata (..), UserType (..), postQualifiedOtrMessage, sendLocalMessages)
import Galley.API.Update (notifyConversationMetadataUpdate)
import qualified Galley.API.Update as API
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Effects
import Galley.Intra.User (getConnections)
import Galley.Types.Conversations.Members (LocalMember (..), defMemberStatus)
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import qualified System.Logger.Class as Log
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Member (OtherMember (..))
import qualified Wire.API.Conversation.Role as Public
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
  ( ConversationUpdate (..),
    GetConversationsRequest (..),
    GetConversationsResponse (..),
    LeaveConversationRequest (..),
    LeaveConversationResponse (..),
    MessageSendRequest (..),
    MessageSendResponse (..),
    NewRemoteConversation (..),
    RemoteMessage (..),
    UserDeletedConversationsNotification,
  )
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Public.Galley.Responses (RemoveFromConversationError (..))
import Wire.API.ServantProto (FromProto (..))
import Wire.API.User.Client (userClientMap)

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) (Galley GalleyEffects)
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      { FederationAPIGalley.onConversationCreated = onConversationCreated,
        FederationAPIGalley.getConversations = getConversations,
        FederationAPIGalley.onConversationUpdated = onConversationUpdated,
        FederationAPIGalley.leaveConversation = leaveConversation,
        FederationAPIGalley.onMessageSent = onMessageSent,
        FederationAPIGalley.sendMessage = sendMessage,
        FederationAPIGalley.onUserDeleted = onUserDeleted
      }

onConversationCreated ::
  Members '[BrigAccess, GundeckAccess, ExternalAccess] r =>
  Domain ->
  NewRemoteConversation ConvId ->
  Galley r ()
onConversationCreated domain rc = do
  let qrc = fmap (toRemoteUnsafe domain) rc
  loc <- qualifyLocal ()
  let (localUserIds, _) = partitionQualified loc (map omQualifiedId (toList (rcNonCreatorMembers rc)))

  addedUserIds <-
    addLocalUsersToRemoteConv
      (rcCnvId qrc)
      (qUntagged (FederationAPIGalley.rcRemoteOrigUserId qrc))
      localUserIds

  let connectedMembers =
        Set.filter
          ( foldQualified
              loc
              (flip Set.member addedUserIds . tUnqualified)
              (const True)
              . omQualifiedId
          )
          (rcNonCreatorMembers rc)
  -- Make sure to notify only about local users connected to the adder
  let qrcConnected = qrc {rcNonCreatorMembers = connectedMembers}

  forM_ (fromNewRemoteConversation loc qrcConnected) $ \(mem, c) -> do
    let event =
          Event
            ConvCreate
            (qUntagged (rcCnvId qrcConnected))
            (qUntagged (FederationAPIGalley.rcRemoteOrigUserId qrcConnected))
            (rcTime qrcConnected)
            (EdConversation c)
    pushConversationEvent Nothing event [qUnqualified . Public.memId $ mem] []

getConversations ::
  Domain ->
  GetConversationsRequest ->
  Galley r GetConversationsResponse
getConversations domain (GetConversationsRequest uid cids) = do
  let ruid = toRemoteUnsafe domain uid
  localDomain <- viewFederationDomain
  GetConversationsResponse
    . mapMaybe (Mapping.conversationToRemote localDomain ruid)
    <$> Data.localConversations cids

getLocalUsers :: Domain -> NonEmpty (Qualified UserId) -> [UserId]
getLocalUsers localDomain = map qUnqualified . filter ((== localDomain) . qDomain) . toList

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
onConversationUpdated ::
  Members '[BrigAccess, GundeckAccess, ExternalAccess] r =>
  Domain ->
  ConversationUpdate ->
  Galley r ()
onConversationUpdated requestingDomain cu = do
  localDomain <- viewFederationDomain
  loc <- qualifyLocal ()
  let rconvId = toRemoteUnsafe requestingDomain (cuConvId cu)
      qconvId = qUntagged rconvId

  -- Note: we generally do not send notifications to users that are not part of
  -- the conversation (from our point of view), to prevent spam from the remote
  -- backend. See also the comment below.
  (presentUsers, allUsersArePresent) <- Data.filterRemoteConvMembers (cuAlreadyPresentUsers cu) qconvId

  -- Perform action, and determine extra notification targets.
  --
  -- When new users are being added to the conversation, we consider them as
  -- notification targets. Once we start checking connections before letting
  -- people being added, this will be safe against spam. However, if users that
  -- are not in the conversations are being removed or have their membership state
  -- updated, we do **not** add them to the list of targets, because we have no
  -- way to make sure that they are actually supposed to receive that notification.
  (mActualAction, extraTargets) <- case cuAction cu of
    ConversationActionAddMembers toAdd role -> do
      let (localUsers, remoteUsers) = partitionQualified loc toAdd
      addedLocalUsers <- Set.toList <$> addLocalUsersToRemoteConv rconvId (cuOrigUserId cu) localUsers
      let allAddedUsers = map (qUntagged . qualifyAs loc) addedLocalUsers <> map qUntagged remoteUsers
      case allAddedUsers of
        [] -> pure (Nothing, []) -- If no users get added, its like no action was performed.
        (u : us) -> pure (Just $ ConversationActionAddMembers (u :| us) role, addedLocalUsers)
    ConversationActionRemoveMembers toRemove -> do
      let localUsers = getLocalUsers localDomain toRemove
      Data.removeLocalMembersFromRemoteConv rconvId localUsers
      pure (Just $ cuAction cu, [])
    ConversationActionRename _ -> pure (Just $ cuAction cu, [])
    ConversationActionMessageTimerUpdate _ -> pure (Just $ cuAction cu, [])
    ConversationActionMemberUpdate _ _ -> pure (Just $ cuAction cu, [])
    ConversationActionReceiptModeUpdate _ -> pure (Just $ cuAction cu, [])
    ConversationActionAccessUpdate _ -> pure (Just $ cuAction cu, [])
    ConversationActionDelete -> do
      Data.removeLocalMembersFromRemoteConv rconvId presentUsers
      pure (Just $ cuAction cu, [])

  unless allUsersArePresent $
    Log.warn $
      Log.field "conversation" (toByteString' (cuConvId cu))
        . Log.field "domain" (toByteString' requestingDomain)
        . Log.msg
          ( "Attempt to send notification about conversation update \
            \to users not in the conversation" ::
              ByteString
          )

  -- Send notifications
  for_ mActualAction $ \action -> do
    let event = conversationActionToEvent (cuTime cu) (cuOrigUserId cu) qconvId action
        targets = nubOrd $ presentUsers <> extraTargets

    -- FUTUREWORK: support bots?
    pushConversationEvent Nothing event targets []

addLocalUsersToRemoteConv ::
  Member BrigAccess r =>
  Remote ConvId ->
  Qualified UserId ->
  [UserId] ->
  Galley r (Set UserId)
addLocalUsersToRemoteConv remoteConvId qAdder localUsers = do
  connStatus <- getConnections localUsers (Just [qAdder]) (Just Accepted)
  let localUserIdsSet = Set.fromList localUsers
      connected = Set.fromList $ fmap csv2From connStatus
      unconnected = Set.difference localUserIdsSet connected
      connectedList = Set.toList connected

  -- FUTUREWORK: Consider handling the discrepancy between the views of the
  -- conversation-owning backend and the local backend
  unless (Set.null unconnected) $
    Log.warn $
      Log.msg ("A remote user is trying to add unconnected local users to a remote conversation" :: Text)
        . Log.field "remote_user" (show qAdder)
        . Log.field "local_unconnected_users" (show unconnected)

  -- Update the local view of the remote conversation by adding only those local
  -- users that are connected to the adder
  Data.addLocalMembersToRemoteConv remoteConvId connectedList
  pure connected

-- FUTUREWORK: actually return errors as part of the response instead of throwing
leaveConversation ::
  Members '[BotAccess, BrigAccess, ExternalAccess, FederatorAccess, FireAndForget, GundeckAccess] r =>
  Domain ->
  LeaveConversationRequest ->
  Galley r LeaveConversationResponse
leaveConversation requestingDomain lc = do
  let leaver = Qualified (lcLeaver lc) requestingDomain
  lcnv <- qualifyLocal (lcConvId lc)
  fmap
    ( LeaveConversationResponse
        . maybe (Left RemoveFromConversationErrorUnchanged) Right
    )
    . runMaybeT
    . void
    . API.updateLocalConversation lcnv leaver Nothing
    . ConversationActionRemoveMembers
    . pure
    $ leaver

-- FUTUREWORK: report errors to the originating backend
-- FUTUREWORK: error handling for missing / mismatched clients
onMessageSent ::
  Members '[BotAccess, GundeckAccess, ExternalAccess] r =>
  Domain ->
  RemoteMessage ConvId ->
  Galley r ()
onMessageSent domain rmUnqualified = do
  let rm = fmap (toRemoteUnsafe domain) rmUnqualified
      convId = qUntagged $ rmConversation rm
      msgMetadata =
        MessageMetadata
          { mmNativePush = rmPush rm,
            mmTransient = rmTransient rm,
            mmNativePriority = rmPriority rm,
            mmData = rmData rm
          }
      recipientMap = userClientMap $ rmRecipients rm
      msgs = toMapOf (itraversed <.> itraversed) recipientMap
  (members, allMembers) <- Data.filterRemoteConvMembers (Map.keys recipientMap) convId
  unless allMembers $
    Log.warn $
      Log.field "conversation" (toByteString' (qUnqualified convId))
        Log.~~ Log.field "domain" (toByteString' (qDomain convId))
        Log.~~ Log.msg
          ( "Attempt to send remote message to local\
            \ users not in the conversation" ::
              ByteString
          )
  localMembers <- sequence $ Map.fromSet mkLocalMember (Set.fromList members)
  void $ sendLocalMessages (rmTime rm) (rmSender rm) (rmSenderClient rm) Nothing convId localMembers msgMetadata msgs
  where
    -- FUTUREWORK: https://wearezeta.atlassian.net/browse/SQCORE-875
    mkLocalMember :: UserId -> Galley r LocalMember
    mkLocalMember m =
      pure $
        LocalMember
          { lmId = m,
            lmService = Nothing,
            lmStatus = defMemberStatus,
            lmConvRoleName = Public.roleNameWireMember
          }

sendMessage ::
  Members '[BotAccess, BrigAccess, FederatorAccess, GundeckAccess, ExternalAccess] r =>
  Domain ->
  MessageSendRequest ->
  Galley r MessageSendResponse
sendMessage originDomain msr = do
  let sender = Qualified (msrSender msr) originDomain
  msg <- either err pure (fromProto (fromBase64ByteString (msrRawMessage msr)))
  MessageSendResponse <$> postQualifiedOtrMessage User sender Nothing (msrConvId msr) msg
  where
    err = throwM . invalidPayload . LT.pack

onUserDeleted ::
  Members '[FederatorAccess, FireAndForget, ExternalAccess, GundeckAccess] r =>
  Domain ->
  UserDeletedConversationsNotification ->
  Galley r EmptyResponse
onUserDeleted origDomain udcn = do
  let deletedUser = toRemoteUnsafe origDomain (FederationAPIGalley.udcnUser udcn)
      untaggedDeletedUser = qUntagged deletedUser
      convIds = FederationAPIGalley.udcnConversations udcn

  spawnMany $
    fromRange convIds <&> \c -> do
      lc <- qualifyLocal c
      mconv <- Data.conversation c
      Data.removeRemoteMembersFromLocalConv c (pure deletedUser)
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
              let action = ConversationActionRemoveMembers (pure untaggedDeletedUser)
                  botsAndMembers = convBotsAndMembers conv
              void $ notifyConversationMetadataUpdate untaggedDeletedUser Nothing lc botsAndMembers action
  pure EmptyResponse
