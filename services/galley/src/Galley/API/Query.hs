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
    getConversationH,
    getConversationRolesH,
    getConversationIdsH,
    getConversationsH,
    getSelfH,
    internalGetMemberH,
    getConversationMetaH,
  )
where

import Data.ByteString.Conversion
import Data.Id
import Data.IdMapping (MappedOrLocalId (Local), partitionMappedOrLocalIds)
import Data.Range
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.Types as Data
import Galley.Types
import Galley.Types.Bot (BotConvView, botConvView)
import Galley.Types.Conversations.Roles
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Utilities

getBotConversationH :: BotId ::: ConvId ::: JSON -> Galley Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  json <$> getBotConversation zbot zcnv

getBotConversation :: BotId -> ConvId -> Galley BotConvView
getBotConversation zbot zcnv = do
  c <- getConversationAndCheckMembershipWithError convNotFound (botUserId zbot) (Local zcnv)
  let cmems = mapMaybe mkMember (toList (Data.convMembers c))
  pure $ botConvView zcnv (Data.convName c) cmems
  where
    mkMember m
      | memId m /= botUserId zbot = Just (OtherMember (memId m) (memService m) (memConvRoleName m))
      | otherwise = Nothing

getConversationH :: UserId ::: OpaqueConvId ::: JSON -> Galley Response
getConversationH (zusr ::: cnv ::: _) = do
  json <$> getConversation zusr cnv

getConversation :: UserId -> OpaqueConvId -> Galley Conversation
getConversation zusr opaqueCnv = do
  cnv <- resolveOpaqueConvId opaqueCnv
  c <- getConversationAndCheckMembership zusr cnv
  conversationView zusr c

getConversationRolesH :: UserId ::: OpaqueConvId ::: JSON -> Galley Response
getConversationRolesH (zusr ::: cnv ::: _) = do
  json <$> getConversationRoles zusr cnv

getConversationRoles :: UserId -> OpaqueConvId -> Galley ConversationRolesList
getConversationRoles zusr opaqueCnv = do
  cnv <- resolveOpaqueConvId opaqueCnv
  void $ getConversationAndCheckMembership zusr cnv
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ ConversationRolesList wireConvRoles

getConversationIdsH :: UserId ::: Maybe OpaqueConvId ::: Range 1 1000 Int32 ::: JSON -> Galley Response
getConversationIdsH (zusr ::: start ::: size ::: _) = do
  json <$> getConversationIds zusr start size

getConversationIds :: UserId -> Maybe OpaqueConvId -> Range 1 1000 Int32 -> Galley (ConversationList OpaqueConvId)
getConversationIds zusr start size = do
  ids <- Data.conversationIdsFrom zusr start size
  pure $ ConversationList (Data.resultSetResult ids) (Data.resultSetType ids == Data.ResultSetTruncated)

getConversationsH :: UserId ::: Maybe (Either (Range 1 32 (List OpaqueConvId)) OpaqueConvId) ::: Range 1 500 Int32 ::: JSON -> Galley Response
getConversationsH (zusr ::: range ::: size ::: _) =
  json <$> getConversations zusr range size

getConversations :: UserId -> Maybe (Either (Range 1 32 (List OpaqueConvId)) OpaqueConvId) -> Range 1 500 Int32 -> Galley (ConversationList Conversation)
getConversations zusr range size =
  withConvIds zusr range size $ \more ids -> do
    (localConvIds, _qualifiedConvIds) <- partitionMappedOrLocalIds <$> traverse resolveOpaqueConvId ids
    -- FUTUREWORK(federation, #1273): fetch remote conversations from other backend
    cs <-
      Data.conversations localConvIds
        >>= filterM removeDeleted
        >>= filterM (pure . isMember (makeIdOpaque zusr) . Data.convMembers)
    flip ConversationList more <$> mapM (conversationView zusr) cs
  where
    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

getSelfH :: UserId ::: ConvId -> Galley Response
getSelfH (zusr ::: cnv) = do
  json <$> getSelf zusr cnv

getSelf :: UserId -> ConvId -> Galley (Maybe Member)
getSelf zusr cnv =
  internalGetMember cnv zusr

internalGetMemberH :: ConvId ::: UserId -> Galley Response
internalGetMemberH (cnv ::: usr) = do
  json <$> internalGetMember cnv usr

internalGetMember :: ConvId -> UserId -> Galley (Maybe Member)
internalGetMember cnv usr = do
  alive <- Data.isConvAlive cnv
  if alive
    then Data.member cnv usr
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

-----------------------------------------------------------------------------
-- Internal

-- | Invoke the given continuation 'k' with a list of conversation IDs
-- which are looked up based on:
--
-- * just limited by size
-- * an (exclusive) starting point (conversation ID) and size
-- * a list of conversation IDs
--
-- The last case returns those conversation IDs which have an associated
-- user. Additionally 'k' is passed in a 'hasMore' indication (which is
-- always false if the third lookup-case is used).
withConvIds ::
  UserId ->
  Maybe (Either (Range 1 32 (List OpaqueConvId)) OpaqueConvId) ->
  Range 1 500 Int32 ->
  (Bool -> [OpaqueConvId] -> Galley a) ->
  Galley a
withConvIds usr range size k = case range of
  Nothing -> do
    r <- Data.conversationIdsFrom usr Nothing (rcast size)
    k (Data.resultSetType r == Data.ResultSetTruncated) (Data.resultSetResult r)
  Just (Right c) -> do
    r <- Data.conversationIdsFrom usr (Just c) (rcast size)
    k (Data.resultSetType r == Data.ResultSetTruncated) (Data.resultSetResult r)
  Just (Left cc) -> do
    ids <- Data.conversationIdsOf usr cc
    k False ids
{-# INLINE withConvIds #-}
