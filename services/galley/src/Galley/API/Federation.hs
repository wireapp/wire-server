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

import Control.Monad.Catch (throwM)
import Control.Monad.Except (runExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.Domain
import Data.Id (ConvId)
import Data.Json.Util (Base64ByteString (..))
import Data.List1 (list1)
import Data.Qualified (Qualified (..))
import Data.Tagged
import qualified Data.Text.Lazy as LT
import Galley.API.Error (invalidPayload)
import qualified Galley.API.Mapping as Mapping
import Galley.API.Message (UserType (..), postQualifiedOtrMessage)
import qualified Galley.API.Update as API
import Galley.API.Util (fromRegisterConversation, pushConversationEvent, viewFederationDomain)
import Galley.App (Galley)
import qualified Galley.Data as Data
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Conversation.Member (OtherMember (..), memId)
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley
  ( ConversationMemberUpdate (..),
    GetConversationsRequest (..),
    GetConversationsResponse (..),
    LeaveConversationRequest (..),
    LeaveConversationResponse (..),
    MessageSendRequest (..),
    MessageSendResponse (..),
    RegisterConversation (..),
    RemoteMessage (..),
  )
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley
import Wire.API.ServantProto (FromProto (..))

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      { FederationAPIGalley.registerConversation = registerConversation,
        FederationAPIGalley.getConversations = getConversations,
        FederationAPIGalley.updateConversationMemberships = updateConversationMemberships,
        FederationAPIGalley.leaveConversation = leaveConversation,
        FederationAPIGalley.receiveMessage = receiveMessage,
        FederationAPIGalley.sendMessage = sendMessage
      }

registerConversation :: RegisterConversation -> Galley ()
registerConversation rc = do
  localDomain <- viewFederationDomain
  let localUsers =
        foldMap (\om -> guard (qDomain (omQualifiedId om) == localDomain) $> omQualifiedId om)
          . rcMembers
          $ rc
      localUserIds = fmap qUnqualified localUsers
  unless (null localUsers) $ do
    Data.addLocalMembersToRemoteConv localUserIds (rcCnvId rc)
  forM_ (fromRegisterConversation localDomain rc) $ \(mem, c) -> do
    let event =
          Event
            ConvCreate
            (rcCnvId rc)
            (rcOrigUserId rc)
            (rcTime rc)
            (EdConversation c)
    pushConversationEvent Nothing event [memId mem] []

getConversations :: GetConversationsRequest -> Galley GetConversationsResponse
getConversations (GetConversationsRequest qUid gcrConvIds) = do
  domain <- viewFederationDomain
  convs <- Data.conversations gcrConvIds
  let convViews = Mapping.conversationViewMaybeQualified domain qUid <$> convs
  pure $ GetConversationsResponse . catMaybes $ convViews

-- | Update the local database with information on conversation members joining
-- or leaving. Finally, push out notifications to local users.
updateConversationMemberships :: ConversationMemberUpdate -> Galley ()
updateConversationMemberships cmu = do
  localDomain <- viewFederationDomain
  let users = case cmuAction cmu of
        FederationAPIGalley.ConversationMembersActionAdd toAdd -> fst <$> toAdd
        FederationAPIGalley.ConversationMembersActionRemove toRemove -> toRemove
      localUsers = filter ((== localDomain) . qDomain) . toList $ users
      localUserIds = qUnqualified <$> localUsers
      targets = nubOrd $ cmuAlreadyPresentUsers cmu <> localUserIds
  event <- case cmuAction cmu of
    FederationAPIGalley.ConversationMembersActionAdd toAdd -> do
      unless (null localUsers) $
        Data.addLocalMembersToRemoteConv localUserIds (cmuConvId cmu)
      let mems = SimpleMembers (map (uncurry SimpleMember) . toList $ toAdd)
      pure $
        Event
          MemberJoin
          (cmuConvId cmu)
          (cmuOrigUserId cmu)
          (cmuTime cmu)
          (EdMembersJoin mems)
    FederationAPIGalley.ConversationMembersActionRemove toRemove -> do
      case localUserIds of
        [] -> pure ()
        (h : t) ->
          Data.removeLocalMembersFromRemoteConv
            (cmuConvId cmu)
            (list1 h t)
      pure $
        Event
          MemberLeave
          (cmuConvId cmu)
          (cmuOrigUserId cmu)
          (cmuTime cmu)
          (EdMembersLeave . QualifiedUserIdList . toList $ toRemove)
  -- FUTUREWORK: support bots?
  -- send notifications
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
receiveMessage :: Domain -> RemoteMessage ConvId -> Galley ()
receiveMessage domain =
  API.postRemoteToLocal
    . fmap (Tagged . (`Qualified` domain))

sendMessage :: Domain -> MessageSendRequest -> Galley MessageSendResponse
sendMessage originDomain msr = do
  let sender = Qualified (msrSender msr) originDomain
  msg <- either err pure (fromProto (fromBase64ByteString (msrRawMessage msr)))
  MessageSendResponse <$> postQualifiedOtrMessage User sender Nothing (msrConvId msr) msg
  where
    err = throwM . invalidPayload . LT.pack
