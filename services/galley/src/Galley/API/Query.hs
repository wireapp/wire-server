{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
  ( getBotConversation,
    getUnqualifiedConversation,
    getConversation,
    getLocalConversationInternal,
    getConversationRoles,
    conversationIdsPageFromUnqualified,
    conversationIdsPageFromV2,
    conversationIdsPageFrom,
    getConversations,
    listConversations,
    iterateConversations,
    getLocalSelf,
    getSelfMember,
    internalGetMember,
    getConversationMeta,
    getConversationByReusableCode,
    ensureGuestLinksEnabled,
    getConversationGuestLinksStatus,
    ensureConvAdmin,
    getMLSSelfConversation,
    getMLSSelfConversationWithError,
    getMLSOne2OneConversationV5,
    getMLSOne2OneConversationV6,
    getMLSOne2OneConversationInternal,
    getMLSOne2OneConversation,
    isMLSOne2OneEstablished,
  )
where

import Cassandra qualified as C
import Control.Lens
import Control.Monad.Extra
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.Code
import Data.CommaSeparatedList
import Data.Domain (Domain)
import Data.Id as Id
import Data.Map qualified as Map
import Data.Maybe
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Galley.API.Error
import Galley.API.MLS
import Galley.API.MLS.Enabled
import Galley.API.MLS.One2One
import Galley.API.MLS.Types
import Galley.API.Mapping
import Galley.API.Mapping qualified as Mapping
import Galley.API.One2One
import Galley.API.Teams.Features.Get
import Galley.API.Util
import Galley.Data.Conversation qualified as Data
import Galley.Data.Conversation.Types qualified as Data
import Galley.Data.Types (Code (codeConversation))
import Galley.Data.Types qualified as Data
import Galley.Effects
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.FederatorAccess qualified as E
import Galley.Effects.ListItems qualified as E
import Galley.Effects.MemberStore qualified as E
import Galley.Effects.TeamStore qualified as E
import Galley.Env
import Galley.Options
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Logger
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.Code
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Role qualified as Public
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client (FederatorClient, getNegotiatedVersion)
import Wire.API.Federation.Error
import Wire.API.Federation.Version qualified as Federation
import Wire.API.MLS.Keys
import Wire.API.Provider.Bot qualified as Public
import Wire.API.Routes.MultiTablePaging qualified as Public
import Wire.API.Team.Feature as Public
import Wire.API.Team.Member (TeamMember, isAdminOrOwner, permissions)
import Wire.API.User
import Wire.HashPassword (HashPassword)
import Wire.RateLimit
import Wire.Sem.Paging.Cassandra

getBotConversation ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (Input (Local ())) r
  ) =>
  BotId ->
  ConvId ->
  Sem r Public.BotConvView
getBotConversation zbot cnv = do
  lcnv <- qualifyLocal cnv
  (c, _) <- getConversationAndMemberWithError @'ConvNotFound (botUserId zbot) lcnv
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
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConvId ->
  Sem r Public.ConversationV8
getUnqualifiedConversation lusr cnv = do
  c <- getConversationAndCheckMembership (tUntagged lusr) (qualifyAs lusr cnv)
  Mapping.conversationViewV8 lusr c

getConversation ::
  forall r.
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r Public.ConversationV8
getConversation lusr cnv = do
  foldQualified
    lusr
    (getUnqualifiedConversation lusr . tUnqualified)
    (getRemoteConversation lusr)
    cnv

getRemoteConversation ::
  ( Member ConversationStore r,
    Member (ErrorS ConvNotFound) r,
    Member (Error FederationError) r,
    Member TinyLog r,
    Member FederatorAccess r
  ) =>
  Local UserId ->
  Remote ConvId ->
  Sem r Public.ConversationV8
getRemoteConversation lusr remoteConvId = do
  conversations <- getRemoteConversations lusr [remoteConvId]
  case conversations of
    [] -> throwS @'ConvNotFound
    [conv] -> pure conv
    _convs -> throw $ FederationUnexpectedBody "expected one conversation, got multiple"

getRemoteConversations ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (ErrorS 'ConvNotFound) r,
    Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  [Remote ConvId] ->
  Sem r [Public.ConversationV8]
getRemoteConversations lusr remoteConvs =
  getRemoteConversationsWithFailures lusr remoteConvs >>= \case
    -- throw first error
    (failed : _, _) -> throwFgcError $ failed
    ([], result) -> pure result

getLocalConversationInternal ::
  ( Member (Input (Local ())) r,
    Member (ErrorS ConvNotFound) r,
    Member ConversationStore r
  ) =>
  ConvId ->
  Sem r Conversation
getLocalConversationInternal cid = do
  lcid <- qualifyLocal cid
  conv <- getConversationWithError lcid
  pure $ conversationView lcid conv

data FailedGetConversationReason
  = FailedGetConversationLocally
  | FailedGetConversationRemotely FederationError

throwFgcrError ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r
  ) =>
  FailedGetConversationReason ->
  Sem r a
throwFgcrError FailedGetConversationLocally = throwS @'ConvNotFound
throwFgcrError (FailedGetConversationRemotely e) = throw e

data FailedGetConversation
  = FailedGetConversation
      [Qualified ConvId]
      FailedGetConversationReason

throwFgcError ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member (Error FederationError) r
  ) =>
  FailedGetConversation ->
  Sem r a
throwFgcError (FailedGetConversation _ r) = throwFgcrError r

failedGetConversationRemotely ::
  [Remote ConvId] -> FederationError -> FailedGetConversation
failedGetConversationRemotely qconvs =
  FailedGetConversation (map tUntagged qconvs) . FailedGetConversationRemotely

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
  ( Member ConversationStore r,
    Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  [Remote ConvId] ->
  Sem r ([FailedGetConversation], [Public.ConversationV8])
getRemoteConversationsWithFailures lusr convs = do
  -- get self member statuses from the database
  statusMap <- E.getRemoteConversationStatus (tUnqualified lusr) convs
  let remoteView :: Remote RemoteConversationV2 -> ConversationV8
      remoteView rconv =
        Mapping.remoteConversationView
          lusr
          ( Map.findWithDefault
              defMemberStatus
              ((.id) <$> rconv)
              statusMap
          )
          rconv
      (locallyFound, locallyNotFound) = partition (flip Map.member statusMap) convs
      localFailures
        | null locallyNotFound = []
        | otherwise = [failedGetConversationLocally (map tUntagged locallyNotFound)]

  -- request conversations from remote backends
  let rpc :: GetConversationsRequest -> FederatorClient 'Galley GetConversationsResponseV2
      rpc req = do
        mFedVersion <- getNegotiatedVersion
        case mFedVersion of
          Nothing -> error "impossible"
          Just fedVersion ->
            if fedVersion < Federation.V2
              then getConversationsResponseToV2 <$> fedClient @'Galley @"get-conversations@v1" req
              else fedClient @'Galley @"get-conversations" req
  resp <-
    E.runFederatedConcurrentlyEither locallyFound $ \someConvs ->
      rpc $ GetConversationsRequest (tUnqualified lusr) (tUnqualified someConvs)
  bimap (localFailures <>) (map remoteView . concat)
    . partitionEithers
    <$> traverse handleFailure resp
  where
    handleFailure ::
      (Member P.TinyLog r) =>
      Either (Remote [ConvId], FederationError) (Remote GetConversationsResponseV2) ->
      Sem r (Either FailedGetConversation [Remote RemoteConversationV2])
    handleFailure (Left (rcids, e)) = do
      P.warn $
        Logger.msg ("Error occurred while fetching remote conversations" :: ByteString)
          . Logger.field "error" (show e)
      pure . Left $ failedGetConversationRemotely (sequenceA rcids) e
    handleFailure (Right c) = pure . Right . traverse (.convs) $ c

getConversationRoles ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r
  ) =>
  Local UserId ->
  ConvId ->
  Sem r Public.ConversationRolesList
getConversationRoles lusr cnv = do
  void $ getConversationAndCheckMembership (tUntagged lusr) (qualifyAs lusr cnv)
  -- NOTE: If/when custom roles are added, these roles should
  --       be merged with the team roles (if they exist)
  pure $ Public.ConversationRolesList wireConvRoles

conversationIdsPageFromUnqualified ::
  (Member (ListItems LegacyPaging ConvId) r) =>
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
--
-- FUTUREWORK: Move the body of this function to 'conversationIdsPageFrom' once
-- support for V2 is dropped.
conversationIdsPageFromV2 ::
  forall p r.
  ( p ~ CassandraPaging,
    ( Member ConversationStore r,
      Member (Error InternalError) r,
      Member (Input Env) r,
      Member (ListItems p ConvId) r,
      Member (ListItems p (Remote ConvId)) r,
      Member P.TinyLog r
    )
  ) =>
  ListGlobalSelfConvs ->
  Local UserId ->
  Public.GetPaginatedConversationIds ->
  Sem r Public.ConvIdsPage
conversationIdsPageFromV2 listGlobalSelf lusr Public.GetMultiTablePageRequest {..} = do
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
      localPage <- localsOnly localDomain pagingState size
      let remainingSize = fromRange size - fromIntegral (length (Public.mtpResults localPage))
      if Public.mtpHasMore localPage || remainingSize <= 0
        then -- We haven't checked the remotes yet, so has_more must always be True here.
          pure (filterOut localPage) {Public.mtpHasMore = True}
        else do
          -- remainingSize <= size and remainingSize >= 1, so it is safe to convert to Range
          remotePage <- remotesOnly Nothing (unsafeRange remainingSize)
          pure $
            remotePage
              { Public.mtpResults =
                  Public.mtpResults (filterOut localPage)
                    <> Public.mtpResults remotePage
              }

    localsOnly ::
      Domain ->
      Maybe C.PagingState ->
      Range 1 1000 Int32 ->
      Sem r Public.ConvIdsPage
    localsOnly localDomain pagingState size =
      pageToConvIdPage Public.PagingLocals
        . fmap (`Qualified` localDomain)
        <$> E.listItems (tUnqualified lusr) pagingState size

    remotesOnly ::
      Maybe C.PagingState ->
      Range 1 1000 Int32 ->
      Sem r Public.ConvIdsPage
    remotesOnly pagingState size =
      pageToConvIdPage Public.PagingRemotes
        . fmap (tUntagged @'QRemote)
        <$> E.listItems (tUnqualified lusr) pagingState size

    pageToConvIdPage :: Public.LocalOrRemoteTable -> C.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
    pageToConvIdPage table page@C.PageWithState {..} =
      Public.MultiTablePage
        { mtpResults = pwsResults,
          mtpHasMore = C.pwsHasMore page,
          mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
        }

    -- MLS self-conversation of this user
    selfConvId = mlsSelfConvId (tUnqualified lusr)
    isNotSelfConv = (/= selfConvId) . qUnqualified

    -- If this is an old client making a request (i.e., a V1 or V2 client), make
    -- sure to filter out the MLS global team conversation and the MLS
    -- self-conversation.
    --
    -- FUTUREWORK: This is yet to be implemented for global team conversations.
    filterOut :: ConvIdsPage -> ConvIdsPage
    filterOut page | listGlobalSelf == ListGlobalSelf = page
    filterOut page =
      page
        { Public.mtpResults = filter isNotSelfConv $ Public.mtpResults page
        }

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
    ( Member ConversationStore r,
      Member (Error InternalError) r,
      Member (Input Env) r,
      Member (ListItems p ConvId) r,
      Member (ListItems p (Remote ConvId)) r,
      Member P.TinyLog r
    )
  ) =>
  Local UserId ->
  Public.GetPaginatedConversationIds ->
  Sem r Public.ConvIdsPage
conversationIdsPageFrom lusr state = do
  -- NOTE: Getting the MLS self-conversation creates it in case it does not
  -- exist yet. This is to ensure it is automatically listed without needing to
  -- create it separately.
  --
  -- Make sure that in case MLS is not configured (the non-existance of the
  -- backend removal key is a proxy for it) the self-conversation is not
  -- returned or attempted to be created; in that case we skip anything related
  -- to it.
  whenM isMLSEnabled $ void $ getMLSSelfConversation lusr
  conversationIdsPageFromV2 ListGlobalSelf lusr state

getConversations ::
  ( Member (Error InternalError) r,
    Member (ListItems LegacyPaging ConvId) r,
    Member ConversationStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe (Range 1 32 (CommaSeparatedList ConvId)) ->
  Maybe ConvId ->
  Maybe (Range 1 500 Int32) ->
  Sem r (Public.ConversationList Public.ConversationV8)
getConversations luser mids mstart msize = do
  ConversationList cs more <- getConversationsInternal luser mids mstart msize
  flip ConversationList more <$> mapM (Mapping.conversationViewV8 luser) cs

getConversationsInternal ::
  ( Member ConversationStore r,
    Member (ListItems LegacyPaging ConvId) r
  ) =>
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
      >>= filterM removeDeleted
      >>= filterM (pure . isMember (tUnqualified luser) . Data.convLocalMembers)
  pure $ Public.ConversationList cs more
  where
    size = fromMaybe (toRange (Proxy @32)) msize

    -- get ids and has_more flag
    getIds ::
      ( Member ConversationStore r,
        Member (ListItems LegacyPaging ConvId) r
      ) =>
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
      (Member ConversationStore r) =>
      Data.Conversation ->
      Sem r Bool
    removeDeleted c
      | Data.isConvDeleted c = E.deleteConversation (Data.convId c) >> pure False
      | otherwise = pure True

listConversations ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
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
  localConversations <- mapM (Mapping.conversationViewV8 luser) localInternalConversations

  (remoteFailures, remoteConversations) <- getRemoteConversationsWithFailures luser remoteIds
  let (failedConvsLocally, failedConvsRemotely) = partitionGetConversationFailures remoteFailures
      failedConvs = failedConvsLocally <> failedConvsRemotely
      fetchedOrFailedRemoteIds = Set.fromList $ map Public.cnvQualifiedId remoteConversations <> failedConvs
      remoteNotFoundRemoteIds = filter (`Set.notMember` fetchedOrFailedRemoteIds) $ map tUntagged remoteIds
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
            <> map (tUntagged . qualifyAs luser) notFoundLocalIds,
        crFailed = failedConvsRemotely
      }
  where
    removeDeleted ::
      (Member ConversationStore r) =>
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
  ( Member (ListItems LegacyPaging ConvId) r,
    Member ConversationStore r
  ) =>
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

internalGetMember ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member (Input (Local ())) r,
    Member MemberStore r
  ) =>
  Qualified ConvId ->
  UserId ->
  Sem r (Maybe Public.Member)
internalGetMember qcnv usr = do
  lusr <- qualifyLocal usr
  lcnv <- ensureLocal lusr qcnv
  getLocalSelf lusr (tUnqualified lcnv)

getSelfMember ::
  forall r.
  ( Member MemberStore r,
    Member ConversationStore r,
    Member (ErrorS ConvNotFound) r,
    Member (Error FederationError) r,
    Member TinyLog r,
    Member FederatorAccess r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r (Maybe Public.Member)
getSelfMember lusr cnv = do
  foldQualified
    lusr
    (getLocalSelf lusr . tUnqualified)
    getRemoteSelfMember
    cnv
  where
    getRemoteSelfMember ::
      Remote ConvId ->
      Sem r (Maybe Public.Member)
    getRemoteSelfMember remoteConvId = do
      conv <- getRemoteConversation lusr remoteConvId
      pure $ Just $ conv.cnvMembers.cmSelf

getLocalSelf ::
  ( Member ConversationStore r,
    Member MemberStore r
  ) =>
  Local UserId ->
  ConvId ->
  Sem r (Maybe Public.Member)
getLocalSelf lusr cnv = do
  do
    alive <- E.isConversationAlive cnv
    if alive
      then Mapping.localMemberToSelf lusr <$$> E.getLocalMember cnv (tUnqualified lusr)
      else Nothing <$ E.deleteConversation cnv

getConversationMeta ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r
  ) =>
  ConvId ->
  Sem r ConversationMetadata
getConversationMeta cnv =
  ifM
    (E.isConversationAlive cnv)
    (E.getConversationMetadata cnv >>= noteS @'ConvNotFound)
    (E.deleteConversation cnv >> throwS @'ConvNotFound)

getConversationByReusableCode ::
  forall r.
  ( Member BrigAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'InvalidConversationPassword) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (Input Opts) r,
    Member HashPassword r,
    Member RateLimit r
  ) =>
  Local UserId ->
  Key ->
  Value ->
  Sem r ConversationCoverView
getConversationByReusableCode lusr key value = do
  c <- verifyReusableCode (RateLimitUser (tUnqualified lusr)) False Nothing (ConversationCode key value Nothing)
  conv <- E.getConversation (codeConversation c) >>= noteS @'ConvNotFound
  ensureConversationAccess (tUnqualified lusr) conv CodeAccess
  ensureGuestLinksEnabled (Data.convTeam conv)
  pure $ coverView c conv
  where
    coverView :: Data.Code -> Data.Conversation -> ConversationCoverView
    coverView c conv =
      ConversationCoverView
        { cnvCoverConvId = Data.convId conv,
          cnvCoverName = Data.convName conv,
          cnvCoverHasPassword = Data.codeHasPassword c
        }

ensureGuestLinksEnabled ::
  forall r.
  ( Member (ErrorS 'GuestLinksDisabled) r,
    Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Maybe TeamId ->
  Sem r ()
ensureGuestLinksEnabled mbTid =
  getConversationGuestLinksFeatureStatus mbTid >>= \ws -> case ws.status of
    FeatureStatusEnabled -> pure ()
    FeatureStatusDisabled -> throwS @'GuestLinksDisabled

getConversationGuestLinksStatus ::
  forall r.
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  UserId ->
  ConvId ->
  Sem r (LockableFeature GuestLinksConfig)
getConversationGuestLinksStatus uid convId = do
  conv <- E.getConversation convId >>= noteS @'ConvNotFound
  mTeamMember <- maybe (pure Nothing) (flip E.getTeamMember uid) conv.convMetadata.cnvmTeam
  ensureConvAdmin conv uid mTeamMember
  getConversationGuestLinksFeatureStatus (Data.convTeam conv)

getConversationGuestLinksFeatureStatus ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Maybe TeamId ->
  Sem r (LockableFeature GuestLinksConfig)
getConversationGuestLinksFeatureStatus Nothing = getFeatureForServer @GuestLinksConfig
getConversationGuestLinksFeatureStatus (Just tid) = getFeatureForTeam @GuestLinksConfig tid

-- | The same as 'getMLSSelfConversation', but it throws an error in case the
-- backend is not configured for MLS (the proxy for it being the existance of
-- the backend removal key).
getMLSSelfConversationWithError ::
  forall r.
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (Input Env) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r ConversationV8
getMLSSelfConversationWithError lusr = do
  assertMLSEnabled
  getMLSSelfConversation lusr

-- | Get an MLS self conversation. In case it does not exist, it is partially
-- created in the database. The part that is not written is the epoch number;
-- the number is inserted only upon the first commit. With this we avoid race
-- conditions where two clients concurrently try to create or update the self
-- conversation, where the only thing that can be updated is bumping the epoch
-- number.
getMLSSelfConversation ::
  forall r.
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r ConversationV8
getMLSSelfConversation lusr = do
  let selfConvId = mlsSelfConvId . tUnqualified $ lusr
  mconv <- E.getConversation selfConvId
  cnv <- maybe (E.createMLSSelfConversation lusr) pure mconv
  conversationViewV8 lusr cnv

-- | Get an MLS 1-1 conversation. If not already existing, the conversation
-- object is created on the fly, but not persisted. The conversation will only
-- be stored in the database when its first commit arrives.
--
-- For the federated case, we do not make the assumption that the other backend
-- uses the same function to calculate the conversation ID and corresponding
-- group ID, however we /do/ assume that the two backends agree on which of the
-- two is responsible for hosting the conversation.
getMLSOne2OneConversationV5 ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Input Env) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'NotConnected) r,
    Member (ErrorS 'MLSFederatedOne2OneNotSupported) r,
    Member FederatorAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r ConversationV8
getMLSOne2OneConversationV5 lself qother = do
  if isLocal lself qother
    then getMLSOne2OneConversationInternal lself qother
    else throwS @MLSFederatedOne2OneNotSupported

getMLSOne2OneConversationInternal ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Input Env) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'NotConnected) r,
    Member FederatorAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r ConversationV8
getMLSOne2OneConversationInternal lself qother =
  (.conversation) <$> getMLSOne2OneConversation lself qother Nothing

getMLSOne2OneConversationV6 ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Input Env) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'NotConnected) r,
    Member FederatorAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r (MLSOne2OneConversation MLSPublicKey)
getMLSOne2OneConversationV6 lself qother = do
  assertMLSEnabled
  ensureConnectedOrSameTeam lself [qother]
  let convId = one2OneConvId BaseProtocolMLSTag (tUntagged lself) qother
  foldQualified
    lself
    (getLocalMLSOne2OneConversation lself)
    (getRemoteMLSOne2OneConversation lself qother)
    convId

getMLSOne2OneConversation ::
  ( Member BrigAccess r,
    Member ConversationStore r,
    Member (Input Env) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'NotConnected) r,
    Member FederatorAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Maybe MLSPublicKeyFormat ->
  Sem r (MLSOne2OneConversation SomeKey)
getMLSOne2OneConversation lself qother fmt = do
  convWithUnformattedKeys <- getMLSOne2OneConversationV6 lself qother
  MLSOne2OneConversation convWithUnformattedKeys.conversation
    <$> formatPublicKeys fmt convWithUnformattedKeys.publicKeys

getLocalMLSOne2OneConversation ::
  ( Member ConversationStore r,
    Member (Error InternalError) r,
    Member P.TinyLog r,
    Member (Input Env) r,
    Member (ErrorS MLSNotEnabled) r
  ) =>
  Local UserId ->
  Local ConvId ->
  Sem r (MLSOne2OneConversation MLSPublicKey)
getLocalMLSOne2OneConversation lself lconv = do
  mconv <- E.getConversation (tUnqualified lconv)
  keys <- mlsKeysToPublic <$$> getMLSPrivateKeys
  conv <- case mconv of
    Nothing -> pure (localMLSOne2OneConversation lself lconv)
    Just conv -> conversationViewV8 lself conv
  pure $
    MLSOne2OneConversation
      { conversation = conv,
        publicKeys = keys
      }

getRemoteMLSOne2OneConversation ::
  ( Member (Error InternalError) r,
    Member (Error FederationError) r,
    Member (ErrorS 'NotConnected) r,
    Member FederatorAccess r,
    Member (ErrorS MLSNotEnabled) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Remote conv ->
  Sem r (MLSOne2OneConversation MLSPublicKey)
getRemoteMLSOne2OneConversation lself qother rconv = do
  -- a conversation can only be remote if it is hosted on the other user's domain
  rother <-
    if qDomain qother == tDomain rconv
      then pure (qualifyAs rconv (qUnqualified qother))
      else throw (InternalErrorWithDescription "Unexpected 1-1 conversation domain")

  resp <-
    E.runFederated rconv $ do
      negotiatedVersion <- getNegotiatedVersion
      case negotiatedVersion of
        Nothing -> error "impossible"
        Just Federation.V0 -> pure . Left . FederationCallFailure $ FederatorClientVersionNegotiationError RemoteTooOld
        Just Federation.V1 -> pure . Left . FederationCallFailure $ FederatorClientVersionNegotiationError RemoteTooOld
        Just _ ->
          fmap Right . fedClient @'Galley @"get-one2one-conversation" $
            GetOne2OneConversationRequest (tUnqualified lself) (tUnqualified rother)
  case resp of
    Right (GetOne2OneConversationV2Ok rc) ->
      pure (remoteMLSOne2OneConversation lself rother rc)
    Right GetOne2OneConversationV2BackendMismatch ->
      throw (FederationUnexpectedBody "Backend mismatch when retrieving a remote 1-1 conversation")
    Right GetOne2OneConversationV2NotConnected -> throwS @'NotConnected
    Right GetOne2OneConversationV2MLSNotEnabled -> do
      -- This is confusing to clients because we do not tell them which backend
      -- doesn't have MLS enabled, which would nice information for fixing
      -- problems in real world. We do the same thing when sending Welcome
      -- messages, so for now, let's do the same thing.
      P.warn $
        Logger.field "domain" (toByteString' (tDomain rother))
          . Logger.msg
            ("Cannot get remote MLSOne2OneConversation because MLS is not enabled on remote" :: ByteString)
      throwS @'MLSNotEnabled
    Left e -> throw e

-- | Check if an MLS 1-1 conversation has been established, namely if its epoch
-- is non-zero. The conversation will only be stored in the database when its
-- first commit arrives.
--
-- For the federated case, we do not make the assumption that the other backend
-- uses the same function to calculate the conversation ID and corresponding
-- group ID, however we /do/ assume that the two backends agree on which of the
-- two is responsible for hosting the conversation.
isMLSOne2OneEstablished ::
  ( Member ConversationStore r,
    Member (Input Env) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'NotConnected) r,
    Member FederatorAccess r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r Bool
isMLSOne2OneEstablished lself qother = do
  assertMLSEnabled
  let convId = one2OneConvId BaseProtocolMLSTag (tUntagged lself) qother
  foldQualified
    lself
    isLocalMLSOne2OneEstablished
    (isRemoteMLSOne2OneEstablished lself qother)
    convId

isLocalMLSOne2OneEstablished ::
  (Member ConversationStore r) =>
  Local ConvId ->
  Sem r Bool
isLocalMLSOne2OneEstablished lconv = do
  mconv <- E.getConversation (tUnqualified lconv)
  pure $ case mconv of
    Nothing -> False
    Just conv -> do
      let meta = fst <$> Data.mlsMetadata conv
      maybe False ((> 0) . epochNumber . cnvmlsEpoch) meta

isRemoteMLSOne2OneEstablished ::
  ( Member (ErrorS 'NotConnected) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member FederatorAccess r,
    Member (ErrorS MLSNotEnabled) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Remote conv ->
  Sem r Bool
isRemoteMLSOne2OneEstablished lself qother rconv = do
  conv <- (.conversation) <$> getRemoteMLSOne2OneConversation lself qother rconv
  pure . (> 0) $ case cnvProtocol conv of
    ProtocolProteus -> 0
    ProtocolMLS meta -> ep meta
    ProtocolMixed meta -> ep meta
  where
    ep :: ConversationMLSData -> Word64
    ep = epochNumber . cnvmlsEpoch

-------------------------------------------------------------------------------
-- Helpers

ensureConvAdmin ::
  ( Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r
  ) =>
  Data.Conversation ->
  UserId ->
  Maybe TeamMember ->
  Sem r ()
ensureConvAdmin conversation uid mTeamMember = do
  case find ((== uid) . lmId) conversation.convLocalMembers of
    Nothing -> throwS @'ConvNotFound
    Just lm -> unless (hasAdminPermissions lm) $ throwS @'ConvAccessDenied
  where
    hasAdminPermissions :: LocalMember -> Bool
    hasAdminPermissions lm = lmConvRoleName lm == roleNameWireAdmin || isChannelAdmin mTeamMember

    isChannelAdmin :: Maybe TeamMember -> Bool
    isChannelAdmin Nothing = False
    isChannelAdmin (Just tm) =
      conversation.convMetadata.cnvmGroupConvType == Just Channel
        && isAdminOrOwner (tm ^. permissions)
