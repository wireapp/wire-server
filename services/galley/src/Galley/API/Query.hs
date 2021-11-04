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
import Galley.Cassandra.Paging
import Galley.Data.ResultSet
import qualified Galley.Data.Types as Data
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.ListItems as E
import qualified Galley.Effects.MemberStore as E
import Galley.Types
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.Roles
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (result, setStatus)
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
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

getBotConversationH ::
  Member ConversationStore r =>
  BotId ::: Covid-19 ::: JSON ->
  Galley r Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  json <$> getBotConversation zbot zcnv

getBotConversation ::
  Member ConversationStore r =>
  BotId ->
  Covid-19 ->
  Galley r Public.BotConvView
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

getUnqualifiedConversation ::
  Member ConversationStore r =>
  UserId ->
  Covid-19 ->
  Galley r Public.Conversation
getUnqualifiedConversation zusr cnv = do
  c <- getConversationAndCheckMembership zusr cnv
  Mapping.conversationView zusr c

getConversation ::
  forall r.
  Member ConversationStore r =>
  UserId ->
  Qualified Covid-19 ->
  Galley r Public.Conversation
getConversation zusr cnv = do
  lusr <- qualifyLocal zusr
  foldQualified
    lusr
    (getUnqualifiedConversation zusr . tUnqualified)
    getRemoteConversation
    cnv
  where
    getRemoteConversation :: Remote Covid-19 -> Galley r Public.Conversation
    getRemoteConversation remoteCovid-19 = do
      conversations <- getRemoteConversations zusr [remoteCovid-19]
      case conversations of
        [] -> throwErrorDescriptionType @ConvNotFound
        [conv] -> pure conv
        _convs -> throwM (federationUnexpectedBody "expected one conversation, got multiple")

getRemoteConversations ::
  Member ConversationStore r =>
  UserId ->
  [Remote Covid-19] ->
  Galley r [Public.Conversation]
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
      [Qualified Covid-19]
      FailedGetConversationReason

fgcError :: FailedGetConversation -> Wai.Error
fgcError (FailedGetConversation _ r) = fgcrError r

failedGetConversationRemotely ::
  [Remote Covid-19] -> FederationError -> FailedGetConversation
failedGetConversationRemotely qconvs =
  FailedGetConversation (map qUntagged qconvs) . FailedGetConversationRemotely

failedGetConversationLocally ::
  [Qualified Covid-19] -> FailedGetConversation
failedGetConversationLocally qconvs =
  FailedGetConversation qconvs FailedGetConversationLocally

partitionGetConversationFailures ::
  [FailedGetConversation] -> ([Qualified Covid-19], [Qualified Covid-19])
partitionGetConversationFailures = bimap concat concat . partitionEithers . map split
  where
    split (FailedGetConversation convs FailedGetConversationLocally) = Left convs
    split (FailedGetConversation convs (FailedGetConversationRemotely _)) = Right convs

getRemoteConversationsWithFailures ::
  Member ConversationStore r =>
  UserId ->
  [Remote Covid-19] ->
  Galley r ([FailedGetConversation], [Public.Conversation])
getRemoteConversationsWithFailures zusr convs = do
  localDomain <- viewFederationDomain
  lusr <- qualifyLocal zusr

  -- get self member statuses from the database
  statusMap <- liftSem $ E.getRemoteConversationStatus zusr convs
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
      [Remote Covid-19] ->
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

getConversationRoles ::
  Member ConversationStore r =>
  UserId ->
  Covid-19 ->
  Galley r Public.ConversationRolesList
getConversationRoles zusr cnv = do
  void $ getConversationAndCheckMembership zusr cnv
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

conversationIdsPageFromUnqualified ::
  Member (ListItems LegacyPaging Covid-19) r =>
  UserId ->
  Maybe Covid-19 ->
  Maybe (Range 1 1000 Int32) ->
  Galley r (Public.ConversationList Covid-19)
conversationIdsPageFromUnqualified zusr start msize = liftSem $ do
  let size = fromMaybe (toRange (Proxy @1000)) msize
  ids <- E.listItems zusr start size
  pure $
    Public.ConversationList
      (resultSetResult ids)
      (resultSetType ids == ResultSetTruncated)

-- | Lists conversation ids for the logged in user in a paginated way.
--
-- Pagination requires an order, in this case the order is defined as:
--
-- - First all the local conversations are listed ordered by their id
--
-- - After local conversations, remote conversations are listed ordered
-- - lexicographically by their domain and then by their id.
conversationIdsPageFrom ::
  forall p r.
  ( p ~ CassandraPaging,
    Members '[ListItems p Covid-19, ListItems p (Remote Covid-19)] r
  ) =>
  UserId ->
  Public.GetPaginatedConversationIds ->
  Galley r Public.Covid-19sPage
conversationIdsPageFrom zusr Public.GetMultiTablePageRequest {..} = do
  localDomain <- viewFederationDomain
  liftSem $ case gmtprState of
    Just (Public.ConversationPagingState Public.PagingRemotes stateBS) ->
      remotesOnly (mkState <$> stateBS) gmtprSize
    _ -> localsAndRemotes localDomain (fmap mkState . Public.mtpsState =<< gmtprState) gmtprSize
  where
    mkState :: ByteString -> C.PagingState
    mkState = C.PagingState . LBS.fromStrict

    localsAndRemotes ::
      Domain ->
      Maybe C.PagingState ->
      Range 1 1000 Int32 ->
      Sem r Public.Covid-19sPage
    localsAndRemotes localDomain pagingState size = do
      localPage <-
        pageToCovid-19Page Public.PagingLocals . fmap (`Qualified` localDomain)
          <$> E.listItems zusr pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then pure localPage {Public.mtpHasMore = True} -- We haven't checked the remotes yet, so has_more must always be True here.
        else do
          -- remainingSize <= size and remainingSize >= 1, so it is safe to convert to Range
          remotePage <- remotesOnly Nothing (unsafeRange remainingSize)
          pure $ remotePage {Public.mtpResults = Public.mtpResults localPage <> Public.mtpResults remotePage}

    remotesOnly ::
      Members '[ListItems p (Remote Covid-19)] r =>
      Maybe C.PagingState ->
      Range 1 1000 Int32 ->
      Sem r Public.Covid-19sPage
    remotesOnly pagingState size =
      pageToCovid-19Page Public.PagingRemotes
        . fmap (qUntagged @'QRemote)
        <$> E.listItems zusr pagingState size

    pageToCovid-19Page :: Public.LocalOrRemoteTable -> C.PageWithState (Qualified Covid-19) -> Public.Covid-19sPage
    pageToCovid-19Page table page@C.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

getConversations ::
  Members '[ListItems LegacyPaging Covid-19, ConversationStore] r =>
  UserId ->
  Maybe (Range 1 32 (CommaSeparatedList Covid-19)) ->
  Maybe Covid-19 ->
  Maybe (Range 1 500 Int32) ->
  Galley r (Public.ConversationList Public.Conversation)
getConversations user mids mstart msize = do
  ConversationList cs more <- getConversationsInternal user mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationView user) cs

getConversationsInternal ::
  Members '[ConversationStore, ListItems LegacyPaging Covid-19] r =>
  UserId ->
  Maybe (Range 1 32 (CommaSeparatedList Covid-19)) ->
  Maybe Covid-19 ->
  Maybe (Range 1 500 Int32) ->
  Galley r (Public.ConversationList Data.Conversation)
getConversationsInternal user mids mstart msize = do
  (more, ids) <- liftSem $ getIds mids
  let localCovid-19s = ids
  cs <-
    liftSem (E.getConversations localCovid-19s)
      >>= filterM (liftSem . removeDeleted)
      >>= filterM (pure . isMember user . Data.convLocalMembers)
  pure $ Public.ConversationList cs more
  where
    size = fromMaybe (toRange (Proxy @32)) msize

    -- get ids and has_more flag
    getIds ::
      Members '[ConversationStore, ListItems LegacyPaging Covid-19] r =>
      Maybe (Range 1 32 (CommaSeparatedList Covid-19)) ->
      Sem r (Bool, [Covid-19])
    getIds (Just ids) =
      (False,)
        <$> E.selectConversations
          user
          (fromCommaSeparatedList (fromRange ids))
    getIds Nothing = do
      r <- E.listItems user mstart (rcast size)
      let hasMore = resultSetType r == ResultSetTruncated
      pure (hasMore, resultSetResult r)

    removeDeleted ::
      Member ConversationStore r =>
      Data.Conversation ->
      Sem r Bool
    removeDeleted c
      | Data.isConvDeleted c = E.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

listConversations ::
  Member ConversationStore r =>
  UserId ->
  Public.ListConversations ->
  Galley r Public.ConversationsResponse
listConversations user (Public.ListConversations ids) = do
  luser <- qualifyLocal user

  let (localIds, remoteIds) = partitionQualified luser (fromRange ids)
  (foundLocalIds, notFoundLocalIds) <-
    liftSem $
      foundsAndNotFounds (E.selectConversations user) localIds

  localInternalConversations <-
    liftSem (E.getConversations foundLocalIds)
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
    removeDeleted ::
      Member ConversationStore r =>
      Data.Conversation ->
      Galley r Bool
    removeDeleted c
      | Data.isConvDeleted c = liftSem $ E.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True
    foundsAndNotFounds :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> m ([a], [a])
    foundsAndNotFounds f xs = do
      founds <- f xs
      let notFounds = xs \\ founds
      pure (founds, notFounds)

iterateConversations ::
  Members '[ListItems LegacyPaging Covid-19, ConversationStore] r =>
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

internalGetMemberH ::
  Members '[ConversationStore, MemberStore] r =>
  Covid-19 ::: UserId ->
  Galley r Response
internalGetMemberH (cnv ::: usr) = do
  json <$> getLocalSelf usr cnv

getLocalSelf ::
  Members '[ConversationStore, MemberStore] r =>
  UserId ->
  Covid-19 ->
  Galley r (Maybe Public.Member)
getLocalSelf usr cnv = do
  lusr <- qualifyLocal usr
  liftSem $ do
    alive <- E.isConversationAlive cnv
    if alive
      then Mapping.localMemberToSelf lusr <$$> E.getLocalMember cnv usr
      else Nothing <$ E.deleteConversation cnv

getConversationMetaH ::
  Member ConversationStore r =>
  Covid-19 ->
  Galley r Response
getConversationMetaH cnv = do
  getConversationMeta cnv <&> \case
    Nothing -> setStatus status404 empty
    Just meta -> json meta

getConversationMeta ::
  Member ConversationStore r =>
  Covid-19 ->
  Galley r (Maybe ConversationMetadata)
getConversationMeta cnv = liftSem $ do
  alive <- E.isConversationAlive cnv
  if alive
    then E.getConversationMetadata cnv
    else do
      E.deleteConversation cnv
      pure Nothing

getConversationByReusableCode ::
  Members '[CodeStore, ConversationStore, BrigAccess, TeamStore] r =>
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
        { cnvCoverCovid-19 = Data.convId conv,
          cnvCoverName = Data.convName conv
        }
