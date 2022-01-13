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
    ensureGuestLinksEnabled,
  )
where

import qualified Cassandra as C
import Control.Lens
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
import Galley.Cassandra.Paging
import qualified Galley.Data.Types as Data
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FederatorAccess as E
import qualified Galley.Effects.ListItems as E
import qualified Galley.Effects.MemberStore as E
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Options
import Galley.Types
import Galley.Types.Conversations.Members
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (Error, result, setStatus)
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Logger
import Wire.API.Conversation (ConversationCoverView (..))
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import Wire.API.ErrorDescription
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public
import qualified Wire.API.Routes.MultiTablePaging as Public
import Wire.API.Team.Feature as Public

getBotConversationH ::
  Members '[ConversationStore, Error ConversationError, Input (Local ())] r =>
  BotId ::: ConvId ::: JSON ->
  Sem r Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  lcnv <- qualifyLocal zcnv
  json <$> getBotConversation zbot lcnv

getBotConversation ::
  Members '[ConversationStore, Error ConversationError] r =>
  BotId ->
  Local ConvId ->
  Sem r Public.BotConvView
getBotConversation zbot lcnv = do
  (c, _) <- getConversationAndMemberWithError ConvNotFound (botUserId zbot) lcnv
  let domain = tDomain lcnv
      cmems = mapMaybe (mkMember domain) (toList (Data.convLocalMembers c))
  pure $ Public.botConvView (tUnqualified lcnv) (Data.convName c) cmems
  where
    mkMember :: Domain -> LocalMember -> Maybe OtherMember
    mkMember domain m
      | lmId m == botUserId zbot =
        Nothing -- no need to list the bot itself
      | otherwise =
        Just (OtherMember (Qualified (lmId m) domain) (lmService m) (lmConvRoleName m))

getUnqualifiedConversation ::
  Members '[ConversationStore, Error ConversationError, Error InternalError, P.TinyLog] r =>
  Local UserId ->
  ConvId ->
  Sem r Public.Conversation
getUnqualifiedConversation lusr cnv = do
  c <- getConversationAndCheckMembership (tUnqualified lusr) (qualifyAs lusr cnv)
  Mapping.conversationView lusr c

getConversation ::
  forall r.
  Members
    '[ ConversationStore,
       Error ConversationError,
       Error FederationError,
       Error InternalError,
       FederatorAccess,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Qualified ConvId ->
  Sem r Public.Conversation
getConversation lusr cnv = do
  foldQualified
    lusr
    (getUnqualifiedConversation lusr . tUnqualified)
    getRemoteConversation
    cnv
  where
    getRemoteConversation :: Remote ConvId -> Sem r Public.Conversation
    getRemoteConversation remoteConvId = do
      conversations <- getRemoteConversations lusr [remoteConvId]
      case conversations of
        [] -> throw ConvNotFound
        [conv] -> pure conv
        -- _convs -> throw (federationUnexpectedBody "expected one conversation, got multiple")
        _convs -> throw $ FederationUnexpectedBody "expected one conversation, got multiple"

getRemoteConversations ::
  Members
    '[ ConversationStore,
       Error ConversationError,
       Error FederationError,
       FederatorAccess,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  [Remote ConvId] ->
  Sem r [Public.Conversation]
getRemoteConversations lusr remoteConvs =
  getRemoteConversationsWithFailures lusr remoteConvs >>= \case
    -- throw first error
    (failed : _, _) -> throwFgcError $ failed
    ([], result) -> pure result

data FailedGetConversationReason
  = FailedGetConversationLocally
  | FailedGetConversationRemotely FederationError

throwFgcrError ::
  Members '[Error ConversationError, Error FederationError] r => FailedGetConversationReason -> Sem r a
throwFgcrError FailedGetConversationLocally = throw ConvNotFound
throwFgcrError (FailedGetConversationRemotely e) = throw e

data FailedGetConversation
  = FailedGetConversation
      [Qualified ConvId]
      FailedGetConversationReason

throwFgcError ::
  Members '[Error ConversationError, Error FederationError] r => FailedGetConversation -> Sem r a
throwFgcError (FailedGetConversation _ r) = throwFgcrError r

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
  Members '[ConversationStore, FederatorAccess, P.TinyLog] r =>
  Local UserId ->
  [Remote ConvId] ->
  Sem r ([FailedGetConversation], [Public.Conversation])
getRemoteConversationsWithFailures lusr convs = do
  -- get self member statuses from the database
  statusMap <- E.getRemoteConversationStatus (tUnqualified lusr) convs
  let remoteView :: Remote RemoteConversation -> Conversation
      remoteView rconv =
        Mapping.remoteConversationView
          lusr
          ( Map.findWithDefault
              defMemberStatus
              (fmap rcnvId rconv)
              statusMap
          )
          rconv
      (locallyFound, locallyNotFound) = partition (flip Map.member statusMap) convs
      localFailures
        | null locallyNotFound = []
        | otherwise = [failedGetConversationLocally (map qUntagged locallyNotFound)]

  -- request conversations from remote backends
  let rpc = fedClient @'Galley @VL @"get-conversations"
  resp <-
    E.runFederatedConcurrentlyEither locallyFound $ \someConvs ->
      rpc $ GetConversationsRequest (tUnqualified lusr) (tUnqualified someConvs)
  bimap (localFailures <>) (map remoteView . concat)
    . partitionEithers
    <$> traverse handleFailure resp
  where
    handleFailure ::
      Members '[P.TinyLog] r =>
      Either (Remote [ConvId], FederationError) (Remote GetConversationsResponse) ->
      Sem r (Either FailedGetConversation [Remote RemoteConversation])
    handleFailure (Left (rcids, e)) = do
      P.warn $
        Logger.msg ("Error occurred while fetching remote conversations" :: ByteString)
          . Logger.field "error" (show e)
      pure . Left $ failedGetConversationRemotely (sequenceA rcids) e
    handleFailure (Right c) = pure . Right . traverse gcresConvs $ c

getConversationRoles ::
  Members '[ConversationStore, Error ConversationError] r =>
  Local UserId ->
  ConvId ->
  Sem r Public.ConversationRolesList
getConversationRoles lusr cnv = do
  void $ getConversationAndCheckMembership (tUnqualified lusr) (qualifyAs lusr cnv)
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

conversationIdsPageFromUnqualified ::
  Member (ListItems LegacyPaging ConvId) r =>
  Local UserId ->
  Maybe ConvId ->
  Maybe (Range 1 1000 Int32) ->
  Sem r (Public.ConversationList ConvId)
conversationIdsPageFromUnqualified lusr start msize = do
  let size = fromMaybe (toRange (Proxy @1000)) msize
  ids <- E.listItems (tUnqualified lusr) start size
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
    Members '[ListItems p ConvId, ListItems p (Remote ConvId)] r
  ) =>
  Local UserId ->
  Public.GetPaginatedConversationIds ->
  Sem r Public.ConvIdsPage
conversationIdsPageFrom lusr Public.GetMultiTablePageRequest {..} = do
  let localDomain = tDomain lusr
  case gmtprState of
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
      Sem r Public.ConvIdsPage
    localsAndRemotes localDomain pagingState size = do
      localPage <-
        pageToConvIdPage Public.PagingLocals . fmap (`Qualified` localDomain)
          <$> E.listItems (tUnqualified lusr) pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then pure localPage {Public.mtpHasMore = True} -- We haven't checked the remotes yet, so has_more must always be True here.
        else do
          -- remainingSize <= size and remainingSize >= 1, so it is safe to convert to Range
          remotePage <- remotesOnly Nothing (unsafeRange remainingSize)
          pure $ remotePage {Public.mtpResults = Public.mtpResults localPage <> Public.mtpResults remotePage}

    remotesOnly ::
      Maybe C.PagingState ->
      Range 1 1000 Int32 ->
      Sem r Public.ConvIdsPage
    remotesOnly pagingState size =
      pageToConvIdPage Public.PagingRemotes
        . fmap (qUntagged @'QRemote)
        <$> E.listItems (tUnqualified lusr) pagingState size

    pageToConvIdPage :: Public.LocalOrRemoteTable -> C.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
    pageToConvIdPage table page@C.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

getConversations ::
  Members '[Error InternalError, ListItems LegacyPaging ConvId, ConversationStore, P.TinyLog] r =>
  Local UserId ->
  Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
  Maybe ConvId ->
  Maybe (Range 1 500 Int32) ->
  Sem r (Public.ConversationList Public.Conversation)
getConversations luser mids mstart msize = do
  ConversationList cs more <- getConversationsInternal luser mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationView luser) cs

getConversationsInternal ::
  Members '[ConversationStore, ListItems LegacyPaging ConvId] r =>
  Local UserId ->
  Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
  Maybe ConvId ->
  Maybe (Range 1 500 Int32) ->
  Sem r (Public.ConversationList Data.Conversation)
getConversationsInternal luser mids mstart msize = do
  (more, ids) <- getIds mids
  let localConvIds = ids
  cs <-
    E.getConversations localConvIds
      >>= filterM (removeDeleted)
      >>= filterM (pure . isMember (tUnqualified luser) . Data.convLocalMembers)
  pure $ Public.ConversationList cs more
  where
    size = fromMaybe (toRange (Proxy @32)) msize

    -- get ids and has_more flag
    getIds ::
      Members '[ConversationStore, ListItems LegacyPaging ConvId] r =>
      Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
      Sem r (Bool, [ConvId])
    getIds (Just ids) =
      (False,)
        <$> E.selectConversations
          (tUnqualified luser)
          (fromCommaSeparatedList (fromRange ids))
    getIds Nothing = do
      r <- E.listItems (tUnqualified luser) mstart (rcast size)
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
  Members '[ConversationStore, Error InternalError, FederatorAccess, P.TinyLog] r =>
  Local UserId ->
  Public.ListConversations ->
  Sem r Public.ConversationsResponse
listConversations luser (Public.ListConversations ids) = do
  let (localIds, remoteIds) = partitionQualified luser (fromRange ids)
  (foundLocalIds, notFoundLocalIds) <-
    foundsAndNotFounds (E.selectConversations (tUnqualified luser)) localIds

  localInternalConversations <-
    E.getConversations foundLocalIds
      >>= filterM removeDeleted
      >>= filterM (pure . isMember (tUnqualified luser) . Data.convLocalMembers)
  localConversations <- mapM (Mapping.conversationView luser) localInternalConversations

  (remoteFailures, remoteConversations) <- getRemoteConversationsWithFailures luser remoteIds
  let (failedConvsLocally, failedConvsRemotely) = partitionGetConversationFailures remoteFailures
      failedConvs = failedConvsLocally <> failedConvsRemotely
      fetchedOrFailedRemoteIds = Set.fromList $ map Public.cnvQualifiedId remoteConversations <> failedConvs
      remoteNotFoundRemoteIds = filter (`Set.notMember` fetchedOrFailedRemoteIds) $ map qUntagged remoteIds
  unless (null remoteNotFoundRemoteIds) $
    -- FUTUREWORK: This implies that the backends are out of sync. Maybe the
    -- current user should be considered removed from this conversation at this
    -- point.
    P.warn $
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
      Sem r Bool
    removeDeleted c
      | Data.isConvDeleted c = E.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True
    foundsAndNotFounds :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> m ([a], [a])
    foundsAndNotFounds f xs = do
      founds <- f xs
      let notFounds = xs \\ founds
      pure (founds, notFounds)

iterateConversations ::
  Members '[ListItems LegacyPaging ConvId, ConversationStore] r =>
  Local UserId ->
  Range 1 500 Int32 ->
  ([Data.Conversation] -> Sem r a) ->
  Sem r [a]
iterateConversations luid pageSize handleConvs = go Nothing
  where
    go mbConv = do
      convResult <- getConversationsInternal luid Nothing mbConv (Just pageSize)
      resultHead <- handleConvs (convList convResult)
      resultTail <- case convList convResult of
        (conv : rest) ->
          if convHasMore convResult
            then go (Just (maximum (Data.convId <$> (conv : rest))))
            else pure []
        _ -> pure []
      pure $ resultHead : resultTail

internalGetMemberH ::
  Members '[ConversationStore, Input (Local ()), MemberStore] r =>
  ConvId ::: UserId ->
  Sem r Response
internalGetMemberH (cnv ::: usr) = do
  lusr <- qualifyLocal usr
  json <$> getLocalSelf lusr cnv

getLocalSelf ::
  Members '[ConversationStore, MemberStore] r =>
  Local UserId ->
  ConvId ->
  Sem r (Maybe Public.Member)
getLocalSelf lusr cnv = do
  do
    alive <- E.isConversationAlive cnv
    if alive
      then Mapping.localMemberToSelf lusr <$$> E.getLocalMember cnv (tUnqualified lusr)
      else Nothing <$ E.deleteConversation cnv

getConversationMetaH ::
  Member ConversationStore r =>
  ConvId ->
  Sem r Response
getConversationMetaH cnv = do
  getConversationMeta cnv <&> \case
    Nothing -> setStatus status404 empty
    Just meta -> json meta

getConversationMeta ::
  Member ConversationStore r =>
  ConvId ->
  Sem r (Maybe ConversationMetadata)
getConversationMeta cnv = do
  alive <- E.isConversationAlive cnv
  if alive
    then E.getConversationMetadata cnv
    else do
      E.deleteConversation cnv
      pure Nothing

getConversationByReusableCode ::
  forall r.
  ( Member BrigAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (Error CodeError) r,
    Member (Error ConversationError) r,
    Member (Error NotATeamMember) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Local UserId ->
  Key ->
  Value ->
  Sem r ConversationCoverView
getConversationByReusableCode lusr key value = do
  c <- verifyReusableCode (ConversationCode key value Nothing)
  conv <- ensureConversationAccess (tUnqualified lusr) (Data.codeConversation c) CodeAccess
  ensureGuestLinksEnabled conv
  pure $ coverView conv
  where
    coverView :: Data.Conversation -> ConversationCoverView
    coverView conv =
      ConversationCoverView
        { cnvCoverConvId = Data.convId conv,
          cnvCoverName = Data.convName conv
        }

-- FUTUREWORK(leif): refactor and make it consistent for all team features
ensureGuestLinksEnabled ::
  forall r.
  ( Member (Error ConversationError) r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Data.Conversation ->
  Sem r ()
ensureGuestLinksEnabled conv = do
  defaultStatus <- getDefaultFeatureStatus
  maybeFeatureStatus <- join <$> TeamFeatures.getFeatureStatusNoConfig @'TeamFeatureGuestLinks `traverse` Data.convTeam conv
  case maybe defaultStatus tfwoStatus maybeFeatureStatus of
    TeamFeatureEnabled -> pure ()
    TeamFeatureDisabled -> throw GuestLinksDisabled
  where
    getDefaultFeatureStatus :: Sem r TeamFeatureStatusValue
    getDefaultFeatureStatus = do
      status <- input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)
      pure $ tfwoapsStatus status
