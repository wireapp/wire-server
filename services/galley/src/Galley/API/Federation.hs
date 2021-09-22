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

import Control.Lens (itraversed, (<.>))
import Control.Monad.Catch (throwM)
import Control.Monad.Except (runExceptT)
import Data.ByteString.Conversion (toByteString')
import Data.Containers.ListUtils (nubOrd)
import Data.Domain
import Data.Id (ConvId, UserId)
import Data.Json.Util (Base64ByteString (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Map.Lens (toMapOf)
import Data.Qualified (Qualified (..), toRemote)
import qualified Data.Set as Set
import Data.Tagged
import qualified Data.Text.Lazy as LT
import Galley.API.Error (invalidPayload)
import qualified Galley.API.Mapping as Mapping
import Galley.API.Message (MessageMetadata (..), UserType (..), postQualifiedOtrMessage, sendLocalMessages)
import qualified Galley.API.Update as API
import Galley.API.Util (fromNewRemoteConversation, pushConversationEvent, viewFederationDomain)
import Galley.App (Galley)
import qualified Galley.Data as Data
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
  )
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley
import Wire.API.ServantProto (FromProto (..))
import Wire.API.User.Client (userClientMap)

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      { FederationAPIGalley.onConversationCreated = onConversationCreated,
        FederationAPIGalley.getConversations = getConversations,
        FederationAPIGalley.onConversationUpdated = onConversationUpdated,
        FederationAPIGalley.leaveConversation = leaveConversation,
        FederationAPIGalley.onMessageSent = onMessageSent,
        FederationAPIGalley.sendMessage = sendMessage
      }

onConversationCreated :: Domain -> NewRemoteConversation ConvId -> Galley ()
onConversationCreated domain rc = do
  let qrc = fmap (`Qualified` domain) rc
  localDomain <- viewFederationDomain
  let localUsers =
        foldMap (\om -> guard (qDomain (omQualifiedId om) == localDomain) $> omQualifiedId om)
          . rcMembers
          $ rc
      localUserIds = fmap qUnqualified localUsers
  unless (null localUsers) $ do
    Data.addLocalMembersToRemoteConv (rcCnvId qrc) localUserIds
  forM_ (fromNewRemoteConversation localDomain qrc) $ \(mem, c) -> do
    let event =
          Event
            ConvCreate
            (rcCnvId qrc)
            (rcOrigUserId rc)
            (rcTime rc)
            (EdConversation c)
    pushConversationEvent Nothing event [Public.memId mem] []

getConversations :: Domain -> GetConversationsRequest -> Galley GetConversationsResponse
getConversations domain (GetConversationsRequest uid cids) = do
  let ruid = toRemote $ Qualified uid domain
  localDomain <- viewFederationDomain
  GetConversationsResponse
    . catMaybes
    . map (Mapping.conversationToRemote localDomain ruid)
    <$> Data.conversations cids

getLocalUsers :: Domain -> NonEmpty (Qualified UserId) -> [UserId]
getLocalUsers localDomain = map qUnqualified . filter ((== localDomain) . qDomain) . toList

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
onConversationUpdated :: Domain -> ConversationUpdate -> Galley ()
onConversationUpdated requestingDomain cu = do
  localDomain <- viewFederationDomain
  let qconvId = Qualified (cuConvId cu) requestingDomain

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
  extraTargets <- case cuAction cu of
    ConversationActionAddMembers toAdd -> do
      let localUsers = getLocalUsers localDomain (fmap fst toAdd)
      Data.addLocalMembersToRemoteConv qconvId localUsers
      pure localUsers
    ConversationActionRemoveMembers toRemove -> do
      let localUsers = getLocalUsers localDomain toRemove
      Data.removeLocalMembersFromRemoteConv qconvId localUsers
      pure []
    ConversationActionRename _ -> pure []
    ConversationActionMessageTimerUpdate _ -> pure []
    ConversationActionMemberUpdate _ -> pure []

  -- Send notifications
  let event = conversationActionToEvent (cuTime cu) (cuOrigUserId cu) qconvId (cuAction cu)
      targets = nubOrd $ presentUsers <> extraTargets

  unless allUsersArePresent $
    Log.warn $
      Log.field "conversation" (toByteString' (cuConvId cu))
        Log.~~ Log.field "domain" (toByteString' requestingDomain)
        Log.~~ Log.msg
          ( "Attempt to send notification about conversation update \
            \to users not in the conversation" ::
              ByteString
          )

  -- FUTUREWORK: support bots?
  pushConversationEvent Nothing event targets []

leaveConversation ::
  Domain ->
  LeaveConversationRequest ->
  Galley LeaveConversationResponse
leaveConversation requestingDomain lc = do
  let leaver = Qualified (lcLeaver lc) requestingDomain
  fmap LeaveConversationResponse . runExceptT . void $
    API.removeMemberFromLocalConv leaver Nothing (lcConvId lc) leaver

-- FUTUREWORK: report errors to the originating backend
-- FUTUREWORK: error handling for missing / mismatched clients
onMessageSent :: Domain -> RemoteMessage ConvId -> Galley ()
onMessageSent domain rmUnqualified = do
  let rm = fmap (Tagged . (`Qualified` domain)) rmUnqualified
  let convId = unTagged $ rmConversation rm
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
    mkLocalMember :: UserId -> Galley LocalMember
    mkLocalMember m =
      pure $
        LocalMember
          { lmId = m,
            lmService = Nothing,
            lmStatus = defMemberStatus,
            lmConvRoleName = Public.roleNameWireMember
          }

sendMessage :: Domain -> MessageSendRequest -> Galley MessageSendResponse
sendMessage originDomain msr = do
  let sender = Qualified (msrSender msr) originDomain
  msg <- either err pure (fromProto (fromBase64ByteString (msrRawMessage msr)))
  MessageSendResponse <$> postQualifiedOtrMessage User sender Nothing (msrConvId msr) msg
  where
    err = throwM . invalidPayload . LT.pack
