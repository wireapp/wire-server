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

import Data.Containers.ListUtils (nubOrd)
import Data.Domain (Domain)
import Data.Id (ClientId, UserId)
import qualified Data.Map as Map
import Data.Qualified (Qualified (..))
import Data.Tagged
import qualified Galley.API.Mapping as Mapping
import Galley.API.Update as API
import Galley.API.Util (fromRegisterConversation, pushConversationEvent, viewFederationDomain)
import Galley.App (Galley)
import qualified Galley.Data as Data
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Conversation.Member (Member, memId)
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley
  ( ConversationMemberUpdate (..),
    GetConversationsRequest (..),
    GetConversationsResponse (..),
    RegisterConversation (..),
    RemoteMessage (..),
  )
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley
import Wire.API.User.Client (QualifiedUserClientMap (..), UserClientMap (..))

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      { FederationAPIGalley.registerConversation = registerConversation,
        FederationAPIGalley.getConversations = getConversations,
        FederationAPIGalley.updateConversationMemberships = updateConversationMemberships,
        FederationAPIGalley.receiveMessage = receiveMessage
      }

registerConversation :: RegisterConversation -> Galley ()
registerConversation rc = do
  localDomain <- viewFederationDomain
  let localUsers = fmap (toQualified localDomain) . getLocals $ localDomain
      localUserIds = map qUnqualified localUsers
  unless (null localUsers) $ do
    Data.addLocalMembersToRemoteConv localUserIds (rcCnvId rc)
  forM_ localUsers $ \usr -> do
    c <- fromRegisterConversation usr rc
    let event =
          Event
            ConvCreate
            (rcCnvId rc)
            (rcOrigUserId rc)
            (rcTime rc)
            (EdConversation c)
    pushConversationEvent event [qUnqualified usr] []
  where
    getLocals :: Domain -> [Member]
    getLocals localDomain = fromMaybe [] . Map.lookup localDomain . rcMembers $ rc
    toQualified :: Domain -> Member -> Qualified UserId
    toQualified domain mem = Qualified (memId mem) domain

getConversations :: GetConversationsRequest -> Galley GetConversationsResponse
getConversations (GetConversationsRequest qUid gcrConvIds) = do
  domain <- viewFederationDomain
  convs <- Data.conversations gcrConvIds
  let convViews = Mapping.conversationViewMaybeQualified domain qUid <$> convs
  pure $ GetConversationsResponse . catMaybes $ convViews

-- FUTUREWORK: also remove users from conversation
updateConversationMemberships :: ConversationMemberUpdate -> Galley ()
updateConversationMemberships cmu = do
  localDomain <- viewFederationDomain
  let localUsers = filter ((== localDomain) . qDomain . fst) (cmuUsersAdd cmu)
      localUserIds = map (qUnqualified . fst) localUsers
  when (not (null localUsers)) $ do
    Data.addLocalMembersToRemoteConv localUserIds (cmuConvId cmu)
  let mems = SimpleMembers (map (uncurry SimpleMember) (cmuUsersAdd cmu))
  let event =
        Event
          MemberJoin
          (cmuConvId cmu)
          (cmuOrigUserId cmu)
          (cmuTime cmu)
          (EdMembersJoin mems)

  -- send notifications
  let targets = nubOrd $ cmuAlreadyPresentUsers cmu <> localUserIds
  -- FUTUREWORK: support bots?
  pushConversationEvent event targets []

-- FUTUREWORK: report errors to the originating backend
receiveMessage :: RemoteMessage -> Galley ()
receiveMessage rm =
  API.postRemoteToLocal
    (rmTime rm)
    (Tagged (rmConversation rm))
    (fmap (,(rmSenderClient rm)) (rmSender rm))
    (rmData rm)
    (expandQUCMap (rmRecipients rm))
  where
    -- TODO: is there an easier way to do this conversion?
    expandQUCMap :: QualifiedUserClientMap a -> [(Qualified (UserId, ClientId), a)]
    expandQUCMap =
      map (\(d, (x, a)) -> (Qualified x d, a))
        . (>>= sequenceA)
        . map (fmap expandUCMap)
        . Map.assocs
        . qualifiedUserClientMap

    expandUCMap :: UserClientMap a -> [((UserId, ClientId), a)]
    expandUCMap =
      map (\(u, (c, a)) -> ((u, c), a))
        . (>>= sequenceA)
        . map (fmap Map.assocs)
        . Map.assocs
        . userClientMap
