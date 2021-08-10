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
{-# LANGUAGE RecordWildCards #-}

module Galley.API.Query
  ( getBotConversationH,
    getUnqualifiedConversation,
    getConversation,
    getConversationRoles,
    conversationIdsPageFromUnqualified,
    conversationIdsPageFrom,
    getConversations,
    listConversations,
    iterateConversations,
    getSelfH,
    internalGetMemberH,
    getConversationMetaH,
    getConversationByReusableCode,
  )
where

import qualified Cassandra as C
import Control.Monad.Catch (throwM)
import qualified Data.ByteString.Lazy as LBS
import Data.Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id as Id
import Data.Proxy
import Data.Qualified (Qualified (..), Remote, partitionRemote, partitionRemoteOrLocalIds', toRemote)
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
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Conversation (ConversationCoverView (..))
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription (convNotFound)
import Wire.API.Federation.API.Galley (gcresConvs)
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public

getBotConversationH :: BotId ::: ConvId ::: JSON -> Galley Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  json <$> getBotConversation zbot zcnv

getBotConversation :: BotId -> ConvId -> Galley Public.BotConvView
getBotConversation zbot zcnv = do
  c <- getConversationAndCheckMembershipWithError (errorDescriptionToWai convNotFound) (botUserId zbot) zcnv
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

getConversation :: UserId -> Qualified ConvId -> Galley Public.Conversation
getConversation zusr cnv = do
  localDomain <- viewFederationDomain
  if qDomain cnv == localDomain
    then getUnqualifiedConversation zusr (qUnqualified cnv)
    else getRemoteConversation zusr (toRemote cnv)

getRemoteConversation :: UserId -> Remote ConvId -> Galley Public.Conversation
getRemoteConversation zusr remoteConvId = do
  conversations <- getRemoteConversations zusr [remoteConvId]
  case conversations of
    [] -> throwErrorDescription convNotFound
    [conv] -> pure conv
    _convs -> throwM (federationUnexpectedBody "expected one conversation, got multiple")

getRemoteConversations :: UserId -> [Remote ConvId] -> Galley [Public.Conversation]
getRemoteConversations zusr remoteConvs = do
  localDomain <- viewFederationDomain
  let qualifiedZUser = Qualified zusr localDomain
  let convsByDomain = partitionRemote remoteConvs
  convs <- pooledForConcurrentlyN 8 convsByDomain $ \(remoteDomain, convIds) -> do
    let req = FederatedGalley.GetConversationsRequest qualifiedZUser convIds
        rpc = FederatedGalley.getConversations FederatedGalley.clientRoutes req
    gcresConvs <$> runFederatedGalley remoteDomain rpc
  pure $ concat convs

getConversationRoles :: UserId -> ConvId -> Galley Public.ConversationRolesList
getConversationRoles zusr cnv = do
  void $ getConversationAndCheckMembership zusr cnv
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

conversationIdsPageFromUnqualified :: UserId -> Maybe ConvId -> Maybe (Range 1 1000 Int32) -> Galley (Public.ConversationList ConvId)
conversationIdsPageFromUnqualified zusr start msize = do
  let size = fromMaybe (toRange (Proxy @1000)) msize
  ids <- Data.conversationIdsFrom zusr start size
  pure $
    Public.ConversationList
      (Data.resultSetResult ids)
      (Data.resultSetType ids == Data.ResultSetTruncated)

-- | Lists conversation ids for the logged in user in a paginated way.
--
-- Pagination requires an order, in this case the order is defined as:
--
-- - First all the local conversations are listed ordered by their id
--
-- - After local conversations, remote conversations are listed ordered
-- - lexicographically by their domain and then by their id.
conversationIdsPageFrom :: UserId -> Public.GetPaginatedConversationIds -> Galley Public.ConvIdsPage
conversationIdsPageFrom zusr Public.GetPaginatedConversationIds {..} = do
  localDomain <- viewFederationDomain
  case gpciPagingState of
    Just (Public.ConversationPagingState Public.PagingRemotes stateBS) -> remotesOnly (mkState <$> stateBS) (fromRange gpciSize)
    _ -> localsAndRemotes localDomain (fmap mkState . Public.cpsPagingState =<< gpciPagingState) gpciSize
  where
    mkState :: ByteString -> C.PagingState
    mkState = C.PagingState . LBS.fromStrict

    localsAndRemotes :: Domain -> Maybe C.PagingState -> Range 1 1000 Int32 -> Galley Public.ConvIdsPage
    localsAndRemotes localDomain pagingState size = do
      localPage <- pageToConvIdPage Public.PagingLocals . fmap (`Qualified` localDomain) <$> Data.localConversationIdsPageFrom zusr pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.pageConvIds localPage))
      if Public.pageHasMore localPage || remainingSize <= 0
        then pure localPage {Public.pageHasMore = True} -- We haven't check the remotes yet, so has_more must always be True here.
        else do
          remotePage <- remotesOnly Nothing remainingSize
          pure $ remotePage {Public.pageConvIds = Public.pageConvIds localPage <> Public.pageConvIds remotePage}

    remotesOnly :: Maybe C.PagingState -> Int32 -> Galley Public.ConvIdsPage
    remotesOnly pagingState size =
      pageToConvIdPage Public.PagingRemotes <$> Data.remoteConversationIdsPageFrom zusr pagingState size

    pageToConvIdPage :: Public.ConversationPagingTable -> Data.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
    pageToConvIdPage table page@Data.PageWithState {..} =
      Public.ConvIdsPage
        { pageConvIds = pwsResults,
          pageHasMore = C.pwsHasMore page,
          pagePagingState =
            Public.ConversationPagingState
              { cpsTable = table,
                cpsPagingState = LBS.toStrict . C.unPagingState <$> pwsState
              }
        }

getConversations :: UserId -> Maybe (Range 1 32 (CommaSeparatedList ConvId)) -> Maybe ConvId -> Maybe (Range 1 500 Int32) -> Galley (Public.ConversationList Public.Conversation)
getConversations user mids mstart msize = do
  ConversationList cs more <- getConversationsInternal user mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationView user) cs

getConversationsInternal :: UserId -> Maybe (Range 1 32 (CommaSeparatedList ConvId)) -> Maybe ConvId -> Maybe (Range 1 500 Int32) -> Galley (Public.ConversationList Data.Conversation)
getConversationsInternal user mids mstart msize = do
  (more, ids) <- getIds mids
  let localConvIds = ids
  -- FUTUREWORK(federation, #1273): fetch remote conversations from other backend
  cs <-
    Data.conversations localConvIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  pure $ Public.ConversationList cs more
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

-- FUTUREWORK: optimize cassandra requests when retrieving conversations (avoid large IN queries, prefer parallel/chunked requests)
listConversations :: UserId -> Public.ListConversations -> Galley Public.ConversationsResponse
listConversations user (Public.ListConversations ids) = do
  localDomain <- viewFederationDomain

  let (remoteConvIds, localIds) = partitionRemoteOrLocalIds' localDomain (toList ids)
  localConvIds <- Data.conversationIdsOf user localIds

  localInternalConversations <-
    Data.conversations localConvIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  localConversations <- mapM (Mapping.conversationView user) localInternalConversations

  remoteConversations <- getRemoteConversations user remoteConvIds
  let allConvs = localConversations <> remoteConversations
  pure $ Public.ConversationsResponse allConvs [] []
  where
    removeDeleted :: Data.Conversation -> Galley Bool
    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

iterateConversations :: forall a. UserId -> Range 1 500 Int32 -> ([Data.Conversation] -> Galley a) -> Galley [a]
iterateConversations uid pageSize handleConvs = go Nothing
  where
    go :: Maybe ConvId -> Galley [a]
    go mbConv = do
      convResult <- getConversationsInternal uid Nothing mbConv (Just pageSize)
      resultHead <- handleConvs (convList convResult)
      resultTail <- case convList convResult of
        (conv : rest) ->
          if convHasMore convResult
            then go (Just (maximum (Data.convId <$> (conv : rest))))
            else pure []
        _ -> pure []
      pure $ resultHead : resultTail

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

getConversationByReusableCode :: UserId -> Key -> Value -> Galley ConversationCoverView
getConversationByReusableCode zusr key value = do
  c <- verifyReusableCode (ConversationCode key value Nothing)
  conv <- ensureConversationAccess zusr (Data.codeConversation c) CodeAccess
  pure $ coverView conv
  where
    coverView :: Data.Conversation -> ConversationCoverView
    coverView conv =
      ConversationCoverView
        { cnvCoverConvId = Data.convId conv,
          cnvCoverName = Data.convName conv
        }
