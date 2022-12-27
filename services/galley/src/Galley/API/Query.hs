{-# LANGUAGE RecordWildCards #-}

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
  ( getBotConversationH,
    getUnqualifiedConversation,
    getConversation,
    getConversationRoles,
    conversationIdsPageFromUnqualified,
    conversationIdsPageFromV2,
    conversationIdsPageFrom,
    getConversations,
    listConversations,
    iterateConversations,
    getLocalSelf,
    internalGetMemberH,
    getConversationMetaH,
    getConversationByReusableCode,
    ensureGuestLinksEnabled,
    getConversationGuestLinksStatus,
    ensureConvAdmin,
    getMLSSelfConversation,
    getMLSSelfConversationWithError,
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
import Galley.API.MLS
import Galley.API.MLS.Keys
import Galley.API.MLS.Types
import Galley.API.Mapping
import qualified Galley.API.Mapping as Mapping
import Galley.API.Util
import qualified Galley.Data.Conversation as Data
import Galley.Data.Types (Code (codeConversation))
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FederatorAccess as E
import qualified Galley.Effects.ListItems as E
import qualified Galley.Effects.MemberStore as E
import Galley.Effects.TeamFeatureStore (FeaturePersistentConstraint)
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Env
import Galley.Options
import Galley.Types.Conversations.Members
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
import Wire.API.Conversation hiding (Member)
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Code
import Wire.API.Conversation.Role
import qualified Wire.API.Conversation.Role as Public
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import qualified Wire.API.Provider.Bot as Public
import qualified Wire.API.Routes.MultiTablePaging as Public
import Wire.API.Team.Feature as Public hiding (setStatus)
import Wire.Sem.Paging.Cassandra

getBotConversationH ::
  Members '[ConversationStore, ErrorS 'ConvNotFound, Input (Local ())] r =>
  BotId ::: ConvId ::: JSON ->
  Sem r Response
getBotConversationH (zbot ::: zcnv ::: _) = do
  lcnv <- qualifyLocal zcnv
  json <$> getBotConversation zbot lcnv

getBotConversation ::
  Members '[ConversationStore, ErrorS 'ConvNotFound] r =>
  BotId ->
  Local ConvId ->
  Sem r Public.BotConvView
getBotConversation zbot lcnv = do
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
  Members
    '[ ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
       Error InternalError,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  ConvId ->
  Sem r Public.Conversation
getUnqualifiedConversation lusr cnv = do
  c <- getConversationAndCheckMembership (tUntagged lusr) (qualifyAs lusr cnv)
  Mapping.conversationView lusr c

getConversation ::
  forall r.
  Members
    '[ ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
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
        [] -> throwS @'ConvNotFound
        [conv] -> pure conv
        -- _convs -> throw (federationUnexpectedBody "expected one conversation, got multiple")
        _convs -> throw $ FederationUnexpectedBody "expected one conversation, got multiple"

getRemoteConversations ::
  Members
    '[ ConversationStore,
       Error FederationError,
       ErrorS 'ConvNotFound,
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
  Members '[ErrorS 'ConvNotFound, Error FederationError] r => FailedGetConversationReason -> Sem r a
throwFgcrError FailedGetConversationLocally = throwS @'ConvNotFound
throwFgcrError (FailedGetConversationRemotely e) = throw e

data FailedGetConversation
  = FailedGetConversation
      [Qualified ConvId]
      FailedGetConversationReason

throwFgcError ::
  Members '[ErrorS 'ConvNotFound, Error FederationError] r => FailedGetConversation -> Sem r a
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
        | otherwise = [failedGetConversationLocally (map tUntagged locallyNotFound)]

  -- request conversations from remote backends
  let rpc = fedClient @'Galley @"get-conversations"
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
  Members '[ConversationStore, ErrorS 'ConvNotFound, ErrorS 'ConvAccessDenied] r =>
  Local UserId ->
  ConvId ->
  Sem r Public.ConversationRolesList
getConversationRoles lusr cnv = do
  void $ getConversationAndCheckMembership (tUntagged lusr) (qualifyAs lusr cnv)
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
--
-- FUTUREWORK: Move the body of this function to 'conversationIdsPageFrom' once
-- support for V2 is dropped.
conversationIdsPageFromV2 ::
  forall p r.
  ( p ~ CassandraPaging,
    Members
      '[ ConversationStore,
         Error InternalError,
         Input Env,
         ListItems p ConvId,
         ListItems p (Remote ConvId),
         P.TinyLog
       ]
      r
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
      localPage <-
        pageToConvIdPage Public.PagingLocals . fmap (`Qualified` localDomain)
          <$> E.listItems (tUnqualified lusr) pagingState size
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
    Members
      '[ ConversationStore,
         Error InternalError,
         Input Env,
         ListItems p ConvId,
         ListItems p (Remote ConvId),
         P.TinyLog
       ]
      r
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
  whenM (isJust <$> getMLSRemovalKey)
    . void
    $ getMLSSelfConversation lusr
  conversationIdsPageFromV2 ListGlobalSelf lusr state

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
      >>= filterM removeDeleted
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
  forall db r.
  ( Member BrigAccess r,
    Member CodeStore r,
    Member ConversationStore r,
    Member (ErrorS 'CodeNotFound) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'GuestLinksDisabled) r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r,
    Member (TeamFeatureStore db) r,
    Member (Input Opts) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Local UserId ->
  Key ->
  Value ->
  Sem r ConversationCoverView
getConversationByReusableCode lusr key value = do
  c <- verifyReusableCode (ConversationCode key value Nothing)
  conv <- E.getConversation (codeConversation c) >>= noteS @'ConvNotFound
  ensureConversationAccess (tUnqualified lusr) conv CodeAccess
  ensureGuestLinksEnabled @db (Data.convTeam conv)
  pure $ coverView conv
  where
    coverView :: Data.Conversation -> ConversationCoverView
    coverView conv =
      ConversationCoverView
        { cnvCoverConvId = Data.convId conv,
          cnvCoverName = Data.convName conv
        }

ensureGuestLinksEnabled ::
  forall db r.
  ( Member (ErrorS 'GuestLinksDisabled) r,
    Member (TeamFeatureStore db) r,
    Member (Input Opts) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Maybe TeamId ->
  Sem r ()
ensureGuestLinksEnabled mbTid =
  getConversationGuestLinksFeatureStatus @db mbTid >>= \ws -> case wsStatus ws of
    FeatureStatusEnabled -> pure ()
    FeatureStatusDisabled -> throwS @'GuestLinksDisabled

getConversationGuestLinksStatus ::
  forall db r.
  ( Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  UserId ->
  ConvId ->
  Sem r (WithStatus GuestLinksConfig)
getConversationGuestLinksStatus uid convId = do
  conv <- E.getConversation convId >>= noteS @'ConvNotFound
  ensureConvAdmin (Data.convLocalMembers conv) uid
  getConversationGuestLinksFeatureStatus @db (Data.convTeam conv)

getConversationGuestLinksFeatureStatus ::
  forall db r.
  ( Member (TeamFeatureStore db) r,
    Member (Input Opts) r,
    FeaturePersistentConstraint db GuestLinksConfig
  ) =>
  Maybe TeamId ->
  Sem r (WithStatus GuestLinksConfig)
getConversationGuestLinksFeatureStatus mbTid = do
  defaultStatus :: WithStatus GuestLinksConfig <- input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)
  case mbTid of
    Nothing -> pure defaultStatus
    Just tid -> do
      mbConfigNoLock <- TeamFeatures.getFeatureConfig @db (Proxy @GuestLinksConfig) tid
      mbLockStatus <- TeamFeatures.getFeatureLockStatus @db (Proxy @GuestLinksConfig) tid
      pure $ computeFeatureConfigForTeamUser mbConfigNoLock mbLockStatus defaultStatus

-- | The same as 'getMLSSelfConversation', but it throws an error in case the
-- backend is not configured for MLS (the proxy for it being the existance of
-- the backend removal key).
getMLSSelfConversationWithError ::
  forall r.
  Members
    '[ ConversationStore,
       Error InternalError,
       ErrorS 'MLSNotEnabled,
       Input Env,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Sem r Conversation
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
  Members
    '[ ConversationStore,
       Error InternalError,
       Input Env,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Sem r Conversation
getMLSSelfConversation lusr = do
  let selfConvId = mlsSelfConvId . tUnqualified $ lusr
  mconv <- E.getConversation selfConvId
  cnv <- maybe (E.createMLSSelfConversation lusr) pure mconv
  conversationView lusr cnv

-------------------------------------------------------------------------------
-- Helpers

ensureConvAdmin :: Members '[ErrorS 'ConvAccessDenied, ErrorS 'ConvNotFound] r => [LocalMember] -> UserId -> Sem r ()
ensureConvAdmin users uid =
  case find ((== uid) . lmId) users of
    Nothing -> throwS @'ConvNotFound
    Just lm -> unless (lmConvRoleName lm == roleNameWireAdmin) $ throwS @'ConvAccessDenied
