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
    getLocalSelf,
    internalGetMemberH,
    getConversationMetaH,
    getConversationByReusableCode,
  )
where

import qualified Cassandra as C
import Control.Lens (sequenceAOf)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LBS
import Data.Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id as Id
import qualified Data.Map as Map
import Data.Proxy
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Galley.API.Error
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.Types as Data
import Galley.Effects
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
import Wire.API.ErrorDescription (ConvNotFound)
import Wire.API.Federation.API.Galley (gcresConvs)
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import Wire.API.Federation.Client (FederationError, executeFederated)
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public
import qualified Wire.API.Routes.MultiTablePaging as Public

getBotConversationH :: BotId ::: ConvId ::: JSON -> Galley r Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  json <$> getBotConversation zbot zcnv

getBotConversation :: BotId -> ConvId -> Galley r Public.BotConvView
getBotConversation zbot zcnv = do
  (c, _) <- getConversationAndMemberWithError (errorDescriptionTypeToWai @ConvNotFound) (botUserId zbot) zcnv
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

getUnqualifiedConversation :: UserId -> ConvId -> Galley r Public.Conversation
getUnqualifiedConversation zusr cnv = do
  c <- getConversationAndCheckMembership zusr cnv
  Mapping.conversationView zusr c

getConversation :: UserId -> Qualified ConvId -> Galley r Public.Conversation
getConversation zusr cnv = do
  lusr <- qualifyLocal zusr
  foldQualified
    lusr
    (getUnqualifiedConversation zusr . tUnqualified)
    getRemoteConversation
    cnv
  where
    getRemoteConversation :: Remote ConvId -> Galley r Public.Conversation
    getRemoteConversation remoteConvId = do
      conversations <- getRemoteConversations zusr [remoteConvId]
      case conversations of
        [] -> throwErrorDescriptionType @ConvNotFound
        [conv] -> pure conv
        _convs -> throwM (federationUnexpectedBody "expected one conversation, got multiple")

getRemoteConversations :: UserId -> [Remote ConvId] -> Galley r [Public.Conversation]
getRemoteConversations zusr remoteConvs =
  getRemoteConversationsWithFailures zusr remoteConvs >>= \case
    -- throw first error
    (failed : _, _) -> throwM (fgcError failed)
    ([], result) -> pure result

data FailedGetConversationReason
  = FailedGetConversationLocally
  | FailedGetConversationRemotely FederationError

fgcrError :: FailedGetConversationReason -> Wai.Error
fgcrError FailedGetConversationLocally = errorDescriptionTypeToWai @ConvNotFound
fgcrError (FailedGetConversationRemotely e) = federationErrorToWai e

data FailedGetConversation
  = FailedGetConversation
      [Qualified ConvId]
      FailedGetConversationReason

fgcError :: FailedGetConversation -> Wai.Error
fgcError (FailedGetConversation _ r) = fgcrError r

failedGetConversationRemotely ::
  [Remote ConvId] -> FederationError -> FailedGetConversation
failedGetConversationRemotely qconvs =
  FailedGetConversation (map qUntagged qconvs) . FailedGetConversationRemotely

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
  Galley r ([FailedGetConversation], [Public.Conversation])
getRemoteConversationsWithFailures zusr convs = do
  localDomain <- viewFederationDomain
  lusr <- qualifyLocal zusr

  -- get self member statuses from the database
  statusMap <- Data.remoteConversationStatus zusr convs
  let remoteView :: Remote FederatedGalley.RemoteConversation -> Maybe Conversation
      remoteView rconv =
        Mapping.remoteConversationView
          lusr
          ( Map.findWithDefault
              defMemberStatus
              (fmap FederatedGalley.rcnvId rconv)
              statusMap
          )
          rconv
      (locallyFound, locallyNotFound) = partition (flip Map.member statusMap) convs
      localFailures
        | null locallyNotFound = []
        | otherwise = [failedGetConversationLocally (map qUntagged locallyNotFound)]

  -- request conversations from remote backends
  liftGalley0
    . fmap (bimap (localFailures <>) concat . partitionEithers)
    . pooledForConcurrentlyN 8 (bucketRemote locallyFound)
    $ \someConvs -> do
      let req = FederatedGalley.GetConversationsRequest zusr (tUnqualified someConvs)
          rpc = FederatedGalley.getConversations FederatedGalley.clientRoutes localDomain req
      handleFailures (sequenceAOf tUnqualifiedL someConvs) $ do
        rconvs <- gcresConvs <$> executeFederated (tDomain someConvs) rpc
        pure $ mapMaybe (remoteView . qualifyAs someConvs) rconvs
  where
    handleFailures ::
      [Remote ConvId] ->
      ExceptT FederationError Galley0 a ->
      Galley0 (Either FailedGetConversation a)
    handleFailures rconvs action = runExceptT
      . withExceptT (failedGetConversationRemotely rconvs)
      . catchE action
      $ \e -> do
        lift . Logger.warn $
          Logger.msg ("Error occurred while fetching remote conversations" :: ByteString)
            . Logger.field "error" (show e)
        throwE e

getConversationRoles :: UserId -> ConvId -> Galley r Public.ConversationRolesList
getConversationRoles zusr cnv = do
  void $ getConversationAndCheckMembership zusr cnv
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

conversationIdsPageFromUnqualified :: UserId -> Maybe ConvId -> Maybe (Range 1 1000 Int32) -> Galley r (Public.ConversationList ConvId)
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
conversationIdsPageFrom :: UserId -> Public.GetPaginatedConversationIds -> Galley r Public.ConvIdsPage
conversationIdsPageFrom zusr Public.GetMultiTablePageRequest {..} = do
  localDomain <- viewFederationDomain
  case gmtprState of
    Just (Public.ConversationPagingState Public.PagingRemotes stateBS) -> remotesOnly (mkState <$> stateBS) (fromRange gmtprSize)
    _ -> localsAndRemotes localDomain (fmap mkState . Public.mtpsState =<< gmtprState) gmtprSize
  where
    mkState :: ByteString -> C.PagingState
    mkState = C.PagingState . LBS.fromStrict

    localsAndRemotes :: Domain -> Maybe C.PagingState -> Range 1 1000 Int32 -> Galley r Public.ConvIdsPage
    localsAndRemotes localDomain pagingState size = do
      localPage <- pageToConvIdPage Public.PagingLocals . fmap (`Qualified` localDomain) <$> Data.localConversationIdsPageFrom zusr pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then pure localPage {Public.mtpHasMore = True} -- We haven't checked the remotes yet, so has_more must always be True here.
        else do
          remotePage <- remotesOnly Nothing remainingSize
          pure $ remotePage {Public.mtpResults = Public.mtpResults localPage <> Public.mtpResults remotePage}

    remotesOnly :: Maybe C.PagingState -> Int32 -> Galley r Public.ConvIdsPage
    remotesOnly pagingState size =
      pageToConvIdPage Public.PagingRemotes <$> Data.remoteConversationIdsPageFrom zusr pagingState size

    pageToConvIdPage :: Public.LocalOrRemoteTable -> Data.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
    pageToConvIdPage table page@Data.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

getConversations ::
  UserId ->
  Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
  Maybe ConvId ->
  Maybe (Range 1 500 Int32) ->
  Galley r (Public.ConversationList Public.Conversation)
getConversations user mids mstart msize = do
  ConversationList cs more <- getConversationsInternal user mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationView user) cs

getConversationsInternal ::
  UserId ->
  Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
  Maybe ConvId ->
  Maybe (Range 1 500 Int32) ->
  Galley r (Public.ConversationList Data.Conversation)
getConversationsInternal user mids mstart msize = do
  (more, ids) <- getIds mids
  let localConvIds = ids
  cs <-
    Data.localConversations localConvIds
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

listConversations ::
  UserId ->
  Public.ListConversations ->
  Galley r Public.ConversationsResponse
listConversations user (Public.ListConversations ids) = do
  luser <- qualifyLocal user

  let (localIds, remoteIds) = partitionQualified luser (fromRange ids)
  (foundLocalIds, notFoundLocalIds) <- foundsAndNotFounds (Data.localConversationIdsOf user) localIds

  localInternalConversations <-
    Data.localConversations foundLocalIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  localConversations <- mapM (Mapping.conversationView user) localInternalConversations

  (remoteFailures, remoteConversations) <- getRemoteConversationsWithFailures user remoteIds
  let (failedConvsLocally, failedConvsRemotely) = partitionGetConversationFailures remoteFailures
      failedConvs = failedConvsLocally <> failedConvsRemotely
      fetchedOrFailedRemoteIds = Set.fromList $ map Public.cnvQualifiedId remoteConversations <> failedConvs
      remoteNotFoundRemoteIds = filter (`Set.notMember` fetchedOrFailedRemoteIds) $ map qUntagged remoteIds
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
            <> map (qUntagged . qualifyAs luser) notFoundLocalIds,
        crFailed = failedConvsRemotely
      }
  where
    removeDeleted :: Data.Conversation -> Galley r Bool
    removeDeleted c
      | Data.isConvDeleted c = Data.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True
    foundsAndNotFounds :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> m ([a], [a])
    foundsAndNotFounds f xs = do
      founds <- f xs
      let notFounds = xs \\ founds
      pure (founds, notFounds)

iterateConversations ::
  UserId ->
  Range 1 500 Int32 ->
  ([Data.Conversation] -> Galley r a) ->
  Galley r [a]
iterateConversations uid pageSize handleConvs = go Nothing
  where
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

internalGetMemberH :: ConvId ::: UserId -> Galley r Response
internalGetMemberH (cnv ::: usr) = do
  json <$> getLocalSelf usr cnv

getLocalSelf :: UserId -> ConvId -> Galley r (Maybe Public.Member)
getLocalSelf usr cnv = do
  lusr <- qualifyLocal usr
  alive <- Data.isConvAlive cnv
  if alive
    then Mapping.localMemberToSelf lusr <$$> Data.member cnv usr
    else Nothing <$ Data.deleteConversation cnv

getConversationMetaH :: ConvId -> Galley r Response
getConversationMetaH cnv = do
  getConversationMeta cnv <&> \case
    Nothing -> setStatus status404 empty
    Just meta -> json meta

getConversationMeta :: ConvId -> Galley r (Maybe ConversationMetadata)
getConversationMeta cnv = do
  alive <- Data.isConvAlive cnv
  localDomain <- viewFederationDomain
  if alive
    then Data.conversationMeta localDomain cnv
    else do
      Data.deleteConversation cnv
      pure Nothing

getConversationByReusableCode ::
  Member BrigAccess r =>
  UserId ->
  Key ->
  Value ->
  Galley r ConversationCoverView
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
