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
    listConversationsV2,
    iterateConversations,
    getLocalSelf,
    internalGetMemberH,
    getConversationMetaH,
    getConversationByReusableCode,
  )
where

import qualified Cassandra as C
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LBS
import Data.Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id as Id
import qualified Data.Map as Map
import Data.Proxy
import Data.Qualified (Qualified (..), Remote, partitionRemote, partitionRemoteOrLocalIds', toRemote)
import Data.Range
import qualified Data.Set as Set
import Data.Tagged (unTagged)
import Galley.API.Error
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.Types as Data
import Galley.Types
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.Roles
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Error as Wai
import qualified System.Logger.Class as Logger
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Conversation (ConversationCoverView (..))
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription (convNotFound)
import Wire.API.Federation.API.Galley (gcresConvs)
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Client (FederationError, executeFederated)
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public
import qualified Wire.API.Routes.MultiTablePaging as Public

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
      | lmId m == botUserId zbot =
        Nothing -- no need to list the bot itself
      | otherwise =
        Just (OtherMember (Qualified (lmId m) domain) (lmService m) (lmConvRoleName m))

getUnqualifiedConversation :: UserId -> ConvId -> Galley Public.Conversation
getUnqualifiedConversation zusr cnv = do
  c <- getConversationAndCheckMembership zusr cnv
  Mapping.conversationView zusr c

getConversation :: UserId -> Qualified ConvId -> Galley Public.Conversation
getConversation zusr cnv = do
  localDomain <- viewFederationDomain
  if qDomain cnv == localDomain
    then getUnqualifiedConversation zusr (qUnqualified cnv)
    else getRemoteConversation (toRemote cnv)
  where
    getRemoteConversation :: Remote ConvId -> Galley Public.Conversation
    getRemoteConversation remoteConvId = do
      conversations <- getRemoteConversations zusr [remoteConvId]
      case conversations of
        [] -> throwErrorDescription convNotFound
        [conv] -> pure conv
        _convs -> throwM (federationUnexpectedBody "expected one conversation, got multiple")

getRemoteConversations :: UserId -> [Remote ConvId] -> Galley [Public.Conversation]
getRemoteConversations zusr remoteConvs =
  getRemoteConversationsWithFailures zusr remoteConvs >>= \case
    -- throw first error
    (failed : _, _) -> throwM (fgcError failed)
    ([], result) -> pure result

data FailedGetConversationReason
  = FailedGetConversationLocally
  | FailedGetConversationRemotely FederationError

fgcrError :: FailedGetConversationReason -> Wai.Error
fgcrError FailedGetConversationLocally = errorDescriptionToWai convNotFound
fgcrError (FailedGetConversationRemotely e) = federationErrorToWai e

data FailedGetConversation
  = FailedGetConversation
      [Qualified ConvId]
      FailedGetConversationReason

fgcError :: FailedGetConversation -> Wai.Error
fgcError (FailedGetConversation _ r) = fgcrError r

failedGetConversationRemotely ::
  [Qualified ConvId] -> FederationError -> FailedGetConversation
failedGetConversationRemotely qconvs =
  FailedGetConversation qconvs . FailedGetConversationRemotely

failedGetConversationLocally ::
  [Qualified ConvId] -> FailedGetConversation
failedGetConversationLocally qconvs =
  FailedGetConversation qconvs FailedGetConversationLocally

partitionGetConversationFailures ::
  [FailedGetConversation] -> ([Qualified ConvId], [Qualified ConvId])
partitionGetConversationFailures = bimap concat concat . partitionEithers . map split
  where
    split (FailedGetConversation convs FailedGetConversationLocally) = Left convs
    split (FailedGetConversation convs (FailedGetConversationRemotely _)) = Right convs

getRemoteConversationsWithFailures ::
  UserId ->
  [Remote ConvId] ->
  Galley ([FailedGetConversation], [Public.Conversation])
getRemoteConversationsWithFailures zusr convs = do
  localDomain <- viewFederationDomain

  -- get self member statuses from the database
  statusMap <- Data.remoteConversationStatus zusr convs
  let remoteView rconv =
        Mapping.remoteConversationView
          zusr
          ( Map.findWithDefault
              defMemberStatus
              (toRemote (cnvmQualifiedId (FederatedGalley.rcnvMetadata rconv)))
              statusMap
          )
          rconv
      (locallyFound, locallyNotFound) = partition (flip Map.member statusMap) convs
      localFailures
        | null locallyNotFound = []
        | otherwise = [failedGetConversationLocally (map unTagged locallyNotFound)]

  -- request conversations from remote backends
  fmap (bimap (localFailures <>) concat . partitionEithers)
    . pooledForConcurrentlyN 8 (partitionRemote locallyFound)
    $ \(domain, someConvs) -> do
      let req = FederatedGalley.GetConversationsRequest zusr someConvs
          rpc = FederatedGalley.getConversations FederatedGalley.clientRoutes localDomain req
      handleFailures (map (flip Qualified domain) someConvs) $ do
        rconvs <- gcresConvs <$> executeFederated domain rpc
        pure $ catMaybes (map remoteView rconvs)
  where
    handleFailures ::
      [Qualified ConvId] ->
      ExceptT FederationError Galley a ->
      Galley (Either FailedGetConversation a)
    handleFailures qconvs action = runExceptT
      . withExceptT (failedGetConversationRemotely qconvs)
      . catchE action
      $ \e -> do
        lift . Logger.warn $
          Logger.msg ("Error occurred while fetching remote conversations" :: ByteString)
            . Logger.field "error" (show e)
        throwE e

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
conversationIdsPageFrom zusr Public.GetMultiTablePageRequest {..} = do
  localDomain <- viewFederationDomain
  case gmtprState of
    Just (Public.ConversationPagingState Public.PagingRemotes stateBS) -> remotesOnly (mkState <$> stateBS) (fromRange gmtprSize)
    _ -> localsAndRemotes localDomain (fmap mkState . Public.mtpsState =<< gmtprState) gmtprSize
  where
    mkState :: ByteString -> C.PagingState
    mkState = C.PagingState . LBS.fromStrict

    localsAndRemotes :: Domain -> Maybe C.PagingState -> Range 1 1000 Int32 -> Galley Public.ConvIdsPage
    localsAndRemotes localDomain pagingState size = do
      localPage <- pageToConvIdPage Public.PagingLocals . fmap (`Qualified` localDomain) <$> Data.localConversationIdsPageFrom zusr pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then pure localPage {Public.mtpHasMore = True} -- We haven't check the remotes yet, so has_more must always be True here.
        else do
          remotePage <- remotesOnly Nothing remainingSize
          pure $ remotePage {Public.mtpResults = Public.mtpResults localPage <> Public.mtpResults remotePage}

    remotesOnly :: Maybe C.PagingState -> Int32 -> Galley Public.ConvIdsPage
    remotesOnly pagingState size =
      pageToConvIdPage Public.PagingRemotes <$> Data.remoteConversationIdsPageFrom zusr pagingState size

    pageToConvIdPage :: Public.LocalOrRemoteTable -> Data.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
    pageToConvIdPage table page@Data.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

getConversations :: UserId -> Maybe (Range 1 32 (CommaSeparatedList ConvId)) -> Maybe ConvId -> Maybe (Range 1 500 Int32) -> Galley (Public.ConversationList Public.Conversation)
getConversations user mids mstart msize = do
  ConversationList cs more <- getConversationsInternal user mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationView user) cs

getConversationsInternal :: UserId -> Maybe (Range 1 32 (CommaSeparatedList ConvId)) -> Maybe ConvId -> Maybe (Range 1 500 Int32) -> Galley (Public.ConversationList Data.Conversation)
getConversationsInternal user mids mstart msize = do
  (more, ids) <- getIds mids
  let localConvIds = ids
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
        <$> Data.localConversationIdsOf
          user
          (fromCommaSeparatedList (fromRange ids))
    getIds Nothing = do
      r <- Data.conversationIdsFrom user mstart (rcast size)
      let hasMore = Data.resultSetType r == Data.ResultSetTruncated
      pure (hasMore, Data.resultSetResult r)

    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

-- | Deprecated. FUTUREWORK(federation): Delete this endpoint
listConversations :: UserId -> Public.ListConversations -> Galley (Public.ConversationList Public.Conversation)
listConversations user (Public.ListConversations mIds qstart msize) = do
  localDomain <- viewFederationDomain
  when (isJust mIds && isJust qstart) $
    throwM (invalidPayload "'start' and 'qualified_ids' are mutually exclusive")
  (localMore, localConvIds, remoteConvIds) <- case mIds of
    Just xs -> do
      let (remoteConvIds, localIds) = partitionRemoteOrLocalIds' localDomain (toList xs)
      (localMore, localConvIds) <- getIdsAndMore localIds
      pure (localMore, localConvIds, remoteConvIds)
    Nothing -> do
      (localMore, localConvIds) <- getAll (localstart localDomain)
      remoteConvIds <- Data.conversationsRemote user
      pure (localMore, localConvIds, remoteConvIds)

  localInternalConversations <-
    Data.conversations localConvIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  localConversations <- mapM (Mapping.conversationView user) localInternalConversations

  remoteConversations <- getRemoteConversations user remoteConvIds
  let allConvs = localConversations <> remoteConversations
  pure $ Public.ConversationList allConvs localMore
  where
    localstart localDomain = case qstart of
      Just start | qDomain start == localDomain -> Just (qUnqualified start)
      _ -> Nothing

    size = fromMaybe (toRange (Proxy @32)) msize

    getIdsAndMore :: [ConvId] -> Galley (Bool, [ConvId])
    getIdsAndMore ids = (False,) <$> Data.localConversationIdsOf user ids

    getAll :: Maybe ConvId -> Galley (Bool, [ConvId])
    getAll mstart = do
      r <- Data.conversationIdsFrom user mstart (rcast size)
      let hasMore = Data.resultSetType r == Data.ResultSetTruncated
      pure (hasMore, Data.resultSetResult r)

    removeDeleted :: Data.Conversation -> Galley Bool
    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

listConversationsV2 :: UserId -> Public.ListConversationsV2 -> Galley Public.ConversationsResponse
listConversationsV2 user (Public.ListConversationsV2 ids) = do
  localDomain <- viewFederationDomain

  let (remoteIds, localIds) = partitionRemoteOrLocalIds' localDomain (fromRange ids)
  (foundLocalIds, notFoundLocalIds) <- foundsAndNotFounds (Data.localConversationIdsOf user) localIds

  localInternalConversations <-
    Data.conversations foundLocalIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  localConversations <- mapM (Mapping.conversationView user) localInternalConversations

  (remoteFailures, remoteConversations) <- getRemoteConversationsWithFailures user remoteIds
  let (failedConvsLocally, failedConvsRemotely) = partitionGetConversationFailures remoteFailures
      failedConvs = failedConvsLocally <> failedConvsRemotely
      fetchedOrFailedRemoteIds = Set.fromList $ map Public.cnvQualifiedId remoteConversations <> failedConvs
      remoteNotFoundRemoteIds = filter (`Set.notMember` fetchedOrFailedRemoteIds) $ map unTagged remoteIds
  unless (null remoteNotFoundRemoteIds) $
    -- FUTUREWORK: This implies that the backends are out of sync. Maybe the
    -- current user should be considered removed from this conversation at this
    -- point.
    Logger.warn $
      Logger.msg ("Some locally found conversation ids were not returned by remotes" :: ByteString)
        . Logger.field "convIds" (show remoteNotFoundRemoteIds)

  let allConvs = localConversations <> remoteConversations
  pure $
    Public.ConversationsResponse
      { crFound = allConvs,
        crNotFound =
          failedConvsLocally
            <> remoteNotFoundRemoteIds
            <> map (`Qualified` localDomain) notFoundLocalIds,
        crFailed = failedConvsRemotely
      }
  where
    removeDeleted :: Data.Conversation -> Galley Bool
    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True
    foundsAndNotFounds :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> m ([a], [a])
    foundsAndNotFounds f xs = do
      founds <- f xs
      let notFounds = xs \\ founds
      pure (founds, notFounds)

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

internalGetMemberH :: ConvId ::: UserId -> Galley Response
internalGetMemberH (cnv ::: usr) = do
  json <$> getLocalSelf usr cnv

getLocalSelf :: UserId -> ConvId -> Galley (Maybe Public.Member)
getLocalSelf usr cnv = do
  alive <- Data.isConvAlive cnv
  if alive
    then Mapping.localMemberToSelf <$$> Data.member cnv usr
    else Nothing <$ Data.deleteConversation cnv

getConversationMetaH :: ConvId -> Galley Response
getConversationMetaH cnv = do
  getConversationMeta cnv <&> \case
    Nothing -> setStatus status404 empty
    Just meta -> json meta

getConversationMeta :: ConvId -> Galley (Maybe ConversationMetadata)
getConversationMeta cnv = do
  alive <- Data.isConvAlive cnv
  localDomain <- viewFederationDomain
  if alive
    then Data.conversationMeta localDomain cnv
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
