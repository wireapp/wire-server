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
import Data.Qualified (Qualified (..))
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util (fromRegisterConversation, pushConversationEvent, viewFederationDomain)
import Galley.App (Galley)
import qualified Galley.Data as Data
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Conversation.Member (OtherMember (..), memId)
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley (ConversationMemberUpdate (..), GetConversationsRequest (..), GetConversationsResponse (..), RegisterConversation (..))
import qualified Wire.API.Federation.API.Galley as FederationAPIGalley

federationSitemap :: ServerT (ToServantApi FederationAPIGalley.Api) Galley
federationSitemap =
  genericServerT $
    FederationAPIGalley.Api
      { FederationAPIGalley.registerConversation = registerConversation,
        FederationAPIGalley.getConversations = getConversations,
        FederationAPIGalley.updateConversationMemberships = updateConversationMemberships
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
  forM_ (fromRegisterConversation rc) $ \(mem, c) -> do
    let event =
          Event
            ConvCreate
            (rcCnvId rc)
            (rcOrigUserId rc)
            (rcTime rc)
            (EdConversation c)
    pushConversationEvent event [memId mem] []

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
