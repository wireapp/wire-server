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

module Galley.API.Query
  ( getBotConversationH,
    getUnqualifiedConversation,
    getConversation,
    getConversationRoles,
    getConversationIds,
    getConversations,
    getSelfH,
    internalGetMemberH,
    getConversationMetaH,
  )
where

import Control.Monad.Catch (throwM)
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id as Id
import Data.Proxy
import Data.Qualified (Qualified (..))
import Data.Range
import Galley.API.Error
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.Types as Data
import Galley.Types
import Galley.Types.Conversations.Roles
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Utilities
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.Federation.API.Galley (gcresConvs)
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public

getBotConversationH :: BotId ::: ConvId ::: JSON -> Galley Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  json <$> getBotConversation zbot zcnv

getBotConversation :: BotId -> ConvId -> Galley Public.BotConvView
getBotConversation zbot zcnv = do
  c <- getConversationAndCheckMembershipWithError convNotFound (botUserId zbot) zcnv
  domain <- viewFederationDomain
  let cmems = mapMaybe (mkMember domain) (toList (Data.convLocalMembers c))
  pure $ Public.botConvView zcnv (Data.convName c) cmems
  where
    mkMember :: Domain -> LocalMember -> Maybe OtherMember
    mkMember domain m
      | memId m == botUserId zbot =
        Nothing -- no need to list the bot itself
      | otherwise =
        Just (OtherMember (Qualified (memId m) domain) (memService m) (memConvRoleName m))

getUnqualifiedConversation :: UserId -> ConvId -> Galley Public.Conversation
getUnqualifiedConversation zusr cnv = do
  c <- getConversationAndCheckMembership zusr cnv
  Mapping.conversationView zusr c

getConversation :: UserId -> Domain -> ConvId -> Galley Public.Conversation
getConversation zusr domain cnv = do
  localDomain <- viewFederationDomain
  if domain == localDomain
    then getUnqualifiedConversation zusr cnv
    else getRemoteConversation zusr (Qualified cnv domain)

getRemoteConversation :: UserId -> Qualified ConvId -> Galley Public.Conversation
getRemoteConversation zusr (Qualified convId remoteDomain) = do
  localDomain <- viewFederationDomain
  let qualifiedZUser = Qualified zusr localDomain
      req = FederatedGalley.GetConversationsRequest qualifiedZUser [convId]
      rpc = FederatedGalley.getConversations FederatedGalley.clientRoutes req
  -- we expect the remote galley to make adequate checks on conversation
  -- membership and here we just pass through the reponse
  conversations <- runFederatedGalley remoteDomain rpc
  case gcresConvs conversations of
    [] -> throwM convNotFound
    [conv] -> pure conv
    _convs -> throwM (federationUnexpectedBody "expected one conversation, got multiple")

getConversationRoles :: UserId -> ConvId -> Galley Public.ConversationRolesList
getConversationRoles zusr cnv = do
  void $ getConversationAndCheckMembership zusr cnv
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

getConversationIds :: UserId -> Maybe ConvId -> Maybe (Range 1 1000 Int32) -> Galley (Public.ConversationList ConvId)
getConversationIds zusr start msize = do
  let size = fromMaybe (toRange (Proxy @1000)) msize
  ids <- Data.conversationIdRowsFrom zusr start size
  pure $
    Public.ConversationList
      (Data.resultSetResult ids)
      (Data.resultSetType ids == Data.ResultSetTruncated)

getConversations :: UserId -> Maybe (Range 1 32 (CommaSeparatedList ConvId)) -> Maybe ConvId -> Maybe (Range 1 500 Int32) -> Galley (Public.ConversationList Public.Conversation)
getConversations user mids mstart msize = do
  (more, ids) <- getIds mids
  let localConvIds = ids
  -- FUTUREWORK(federation, #1273): fetch remote conversations from other backend
  cs <-
    Data.conversations localConvIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  flip Public.ConversationList more <$> mapM (Mapping.conversationView user) cs
  where
    size = fromMaybe (toRange (Proxy @32)) msize

    -- get ids and has_more flag
    getIds (Just ids) =
      (False,)
        <$> Data.conversationIdsOf
          user
          (fromCommaSeparatedList (fromRange ids))
    getIds Nothing = do
      r <- Data.conversationIdsFrom user mstart (rcast size)
      let hasMore = Data.resultSetType r == Data.ResultSetTruncated
      pure (hasMore, Data.resultSetResult r)

    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

getSelfH :: UserId ::: ConvId -> Galley Response
getSelfH (zusr ::: cnv) = do
  json <$> getSelf zusr cnv

getSelf :: UserId -> ConvId -> Galley (Maybe Public.Member)
getSelf zusr cnv =
  internalGetMember cnv zusr

internalGetMemberH :: ConvId ::: UserId -> Galley Response
internalGetMemberH (cnv ::: usr) = do
  json <$> internalGetMember cnv usr

internalGetMember :: ConvId -> UserId -> Galley (Maybe Public.Member)
internalGetMember cnv usr = do
  alive <- Data.isConvAlive cnv
  if alive
    then do
      fmap Mapping.toMember <$> Data.member cnv usr
    else do
      Data.deleteConversation cnv
      pure Nothing

getConversationMetaH :: ConvId -> Galley Response
getConversationMetaH cnv = do
  getConversationMeta cnv <&> \case
    Nothing -> setStatus status404 empty
    Just meta -> json meta

getConversationMeta :: ConvId -> Galley (Maybe ConversationMeta)
getConversationMeta cnv = do
  alive <- Data.isConvAlive cnv
  if alive
    then Data.conversationMeta cnv
    else do
      Data.deleteConversation cnv
      pure Nothing
