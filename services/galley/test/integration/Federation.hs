{-# LANGUAGE RecordWildCards #-}

module Federation where

import API.Util
import Bilge.Assert
import Bilge.Response
import qualified Cassandra as C
import Cassandra.Exec (x1)
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Control.Monad.Codensity (lowerCodensity)
import qualified Data.ByteString as LBS
import Data.Domain
import Data.Id
import Data.List.NonEmpty
import qualified Data.List1 as List1
import Data.Qualified
import qualified Data.Set as Set
import Data.Singletons
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Federator.MockServer
import Galley.API.Util
import Galley.Cassandra.Queries
import qualified Galley.Data.Conversation.Types as Types
import Galley.Env
import Galley.Monad
import Galley.Options
import Galley.Run
import Galley.Types.Conversations.Members (LocalMember (..), RemoteMember (..), defMemberStatus, localMemberToOther)
import Imports
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestSetup
import UnliftIO.Retry
import Wire.API.Conversation
import qualified Wire.API.Conversation as Public
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol (Protocol (..))
import Wire.API.Conversation.Role (roleNameWireAdmin, roleNameWireMember)
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley (ConversationUpdate (..), GetConversationsResponse (..))
import Wire.API.Internal.Notification
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.MultiTablePaging
import qualified Wire.API.Routes.MultiTablePaging as Public
import Wire.API.User.Search

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

isConvMemberLTests :: TestM ()
isConvMemberLTests = do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      remoteDomain = Domain "far-away.example.com"
      convId = Id $ fromJust $ UUID.fromString "8cc34301-6949-46c5-bb93-00a72268e2f5"
      convLocalMembers = [LocalMember userId defMemberStatus Nothing roleNameWireMember]
      convRemoteMembers = [RemoteMember rUserId roleNameWireMember]
      lconv =
        toLocalUnsafe localDomain $
          Types.Conversation
            convId
            convLocalMembers
            convRemoteMembers
            False
            (defConversationMetadata userId)
            ProtocolProteus
      lUserId :: Local UserId
      lUserId = toLocalUnsafe localDomain $ Id $ fromJust $ UUID.fromString "217352c0-8b2b-4653-ac76-a88d19490dad" -- A random V4 UUID
      userId = qUnqualified $ tUntagged lUserId
      rUserId :: Remote UserId
      rUserId = toRemoteUnsafe remoteDomain $ Id $ fromJust $ UUID.fromString "d87745f5-dfe7-4ff0-8772-b9c22118b372"
  liftIO $ assertBool "UserId" $ isConvMemberL lconv userId
  liftIO $ assertBool "Local UserId" $ isConvMemberL lconv lUserId
  liftIO $ assertBool "Remote UserId" $ isConvMemberL lconv rUserId
  liftIO $ assertBool "Qualified UserId (local)" $ isConvMemberL lconv $ tUntagged lUserId
  liftIO $ assertBool "Qualified UserId (remote)" $ isConvMemberL lconv $ tUntagged rUserId

updateFedDomainsTest :: TestM ()
updateFedDomainsTest = do
  s <- ask
  let opts = s ^. tsGConf
  -- Don't need the actual server, and we certainly don't want it running.
  -- But this is how the env is made, so it is what we do
  l <- liftIO $ mkLogger (opts ^. optLogLevel) (opts ^. optLogNetStrings) (opts ^. optLogFormat)
  r <- newIORef defFederationDomainConfigs
  (_, env) <- liftIO $ lowerCodensity $ mkApp opts r l
  -- Common variables.
  let interval = (maxBound :: Int) `div` 2 -- Very large values so that we don't have to worry about automatic updates
      remoteDomain = Domain "far-away.example.com"
      remoteDomain2 = Domain "far-away-two.example.com"
  liftIO $ assertBool "remoteDomain is different to local domain" $ remoteDomain /= opts ^. optSettings . setFederationDomain
  liftIO $ assertBool "remoteDomain2 is different to local domain" $ remoteDomain2 /= opts ^. optSettings . setFederationDomain
  -- Setup a conversation for a known remote domain.
  -- Include that domain in the old and new lists so
  -- if the function is acting up we know it will be
  -- working on the domain.
  updateFedDomainsTestNoop env remoteDomain interval

  -- Adding a new federation domain, this too should be a no-op
  updateFedDomainsAddRemote env remoteDomain remoteDomain2 interval

  -- Remove a remote domain from local conversations
  updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval

  -- Remove a local domain from remote conversations
  updateFedDomainRemoveLocalFromRemote env remoteDomain interval

fromFedList :: FederationDomainConfigs -> Set Domain
fromFedList = Set.fromList . fmap domain . remotes

deleteFederationDomains :: FederationDomainConfigs -> FederationDomainConfigs -> App ()
deleteFederationDomains old new = do
  let prev = fromFedList old
      curr = fromFedList new
      deletedDomains = Set.difference prev curr
  -- Call into the galley code
  for_ deletedDomains deleteFederationDomain

constHandlers :: MonadIO m => [RetryStatus -> Handler m Bool]
constHandlers = [const $ Handler $ (\(_ :: SomeException) -> pure True)]

updateFedDomainRemoveRemoteFromLocal :: Env -> Domain -> Domain -> Int -> TestM ()
updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval = recovering x3 constHandlers $ const $ do
  let new = FederationDomainConfigs AllowDynamic [FederationDomainConfig remoteDomain2 FullSearch] interval
      old = new {remotes = FederationDomainConfig remoteDomain FullSearch : remotes new}
  qalice <- randomQualifiedUser
  bobId <- randomId
  charlieId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
      remoteCharlie = Qualified charlieId remoteDomain2
  -- Create a local conversation
  conv <- postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qConvId = decodeQualifiedConvId conv
  connectWithRemoteUser alice remoteBob
  connectWithRemoteUser alice remoteCharlie
  _ <- postQualifiedMembers alice (remoteCharlie <| remoteBob :| []) qConvId
  -- Remove the remote user from the local domain
  liftIO $ runApp env $ deleteFederationDomains old new
  -- Check that the conversation still exists.
  getConvQualified alice qConvId !!! do
    const 200 === statusCode
    let findRemote :: Qualified UserId -> Conversation -> Maybe (Qualified UserId)
        findRemote u = find (== u) . fmap omQualifiedId . cmOthers . cnvMembers
    -- Check that only one remote user was removed.
    const (Right Nothing) === (fmap (findRemote remoteBob) <$> responseJsonEither)
    const (Right $ pure remoteCharlie) === (fmap (findRemote remoteCharlie) <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)

updateFedDomainRemoveLocalFromRemote :: Env -> Domain -> Int -> TestM ()
updateFedDomainRemoveLocalFromRemote env remoteDomain interval = recovering x3 constHandlers $ const $ do
  c <- view tsCannon
  let new = FederationDomainConfigs AllowDynamic [] interval
      old = new {remotes = FederationDomainConfig remoteDomain FullSearch : remotes new}
  -- Make our users
  qalice <- randomQualifiedUser
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  let alice = qUnqualified qalice
      update = memberUpdate {mupHidden = Just False}
  -- Create a remote conversation
  -- START: code from putRemoteConvMemberOk
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu
  -- Expected member state
  let memberAlice =
        Member
          { memId = qalice,
            memService = Nothing,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = roleNameWireMember
          }
  -- Update member state & verify push notification
  WS.bracketR c alice $ \ws -> do
    putMember alice update qconv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qalice
      case evtData e of
        EdMemberUpdate mis -> do
          assertEqual "otr_muted_status" (mupOtrMuteStatus update) (misOtrMutedStatus mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x

  -- Fetch remote conversation
  let bobAsLocal =
        LocalMember
          (qUnqualified qbob)
          defMemberStatus
          Nothing
          roleNameWireAdmin
  let mockConversation =
        mkProteusConv
          (qUnqualified qconv)
          (qUnqualified qbob)
          roleNameWireMember
          [localMemberToOther remoteDomain bobAsLocal]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  (rs, _) <-
    withTempMockFederator'
      (mockReply remoteConversationResponse)
      $ getConvQualified alice qconv
        <!! const 200 === statusCode
  -- Verify new member state
  let alice' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust alice')
    let newAlice = fromJust alice'
    assertEqual "id" (memId memberAlice) (memId newAlice)
    assertEqual "otr_muted_status" (memOtrMutedStatus memberAlice) (memOtrMutedStatus newAlice)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberAlice) (memOtrMutedRef newAlice)
    assertEqual "otr_archived" (memOtrArchived memberAlice) (memOtrArchived newAlice)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberAlice) (memOtrArchivedRef newAlice)
    assertEqual "hidden" (memHidden memberAlice) (memHidden newAlice)
    assertEqual "hidden_ref" (memHiddenRef memberAlice) (memHiddenRef newAlice)
  -- END: code from putRemoteConvMemberOk

  -- Remove the remote user from the local domain
  liftIO $ runApp env $ deleteFederationDomains old new

  -- Get the list of remote conversations for the user.
  -- convIds <- liftIO $ evalGalleyToIO env $
  --   pageToConvIdPage Public.PagingRemotes
  --   . fmap (tUntagged @'QRemote)
  --   <$> E.listItems alice Nothing (toRange $ Proxy @1000)

  convIds <-
    liftIO $
      C.runClient (env ^. cstate) $
        C.retry x1 $
          C.query selectUserRemoteConvs (C.params C.LocalQuorum (pure alice))
  case find (== qUnqualified qconv) $ snd <$> convIds of
    Nothing -> pure ()
    Just c' -> liftIO $ assertFailure $ "Found conversation where none was expected: " <> show c'

pageToConvIdPage :: Public.LocalOrRemoteTable -> C.PageWithState (Qualified ConvId) -> Public.ConvIdsPage
pageToConvIdPage table page@C.PageWithState {..} =
  Public.MultiTablePage
    { mtpResults = pwsResults,
      mtpHasMore = C.pwsHasMore page,
      mtpPagingState = Public.ConversationPagingState table (LBS.toStrict . C.unPagingState <$> pwsState)
    }

updateFedDomainsAddRemote :: Env -> Domain -> Domain -> Int -> TestM ()
updateFedDomainsAddRemote env remoteDomain remoteDomain2 interval = do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      old = FederationDomainConfigs AllowDynamic [FederationDomainConfig remoteDomain FullSearch] interval
      new = old {remotes = FederationDomainConfig remoteDomain2 FullSearch : remotes old}
      -- Just check against the domains, as the search
      -- strategies are outside of this testing scope
      newDoms = domain <$> new.remotes
      oldDoms = domain <$> old.remotes
  liftIO $ assertBool "old and new are different" $ oldDoms /= newDoms
  liftIO $ assertBool "old is shorter than new" $ Imports.length oldDoms < Imports.length newDoms
  liftIO $ assertBool "new contains old" $ all (`elem` newDoms) oldDoms
  liftIO $ assertBool "new elements not in old" $ any (`notElem` oldDoms) newDoms
  qalice <- randomQualifiedUser
  bobId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
  -- Create a conversation

  conv <- postConv alice [] (Just "remote gossip") [] Nothing Nothing
  -- liftIO $ assertBool ("conv = " <> show conv) False
  let convId = decodeConvId conv
  let qConvId = Qualified convId localDomain
  connectWithRemoteUser alice remoteBob
  _ <- postQualifiedMembers alice (remoteBob :| []) qConvId

  -- No-op
  liftIO $ runApp env $ deleteFederationDomains old new
  -- Check that the conversation still exists.
  getConvQualified (qUnqualified qalice) (Qualified convId localDomain) !!! do
    const 200 === statusCode
    let findRemote :: Conversation -> Maybe (Qualified UserId)
        findRemote = find (== remoteBob) . fmap omQualifiedId . cmOthers . cnvMembers
    const (Right $ pure remoteBob) === (fmap findRemote <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)

updateFedDomainsTestNoop :: Env -> Domain -> Int -> TestM ()
updateFedDomainsTestNoop env remoteDomain interval = do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      old = FederationDomainConfigs AllowDynamic [FederationDomainConfig remoteDomain FullSearch] interval
      new = old
  qalice <- randomQualifiedUser
  bobId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
  -- Create a conversation
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qConvId = Qualified convId localDomain
  connectWithRemoteUser alice remoteBob
  _ <- postQualifiedMembers alice (remoteBob :| []) qConvId
  -- No-op
  liftIO $ runApp env $ deleteFederationDomains old new
  -- Check that the conversation still exists.
  getConvQualified (qUnqualified qalice) (Qualified convId localDomain) !!! do
    const 200 === statusCode
    let findRemote :: Conversation -> Maybe (Qualified UserId)
        findRemote = find (== remoteBob) . fmap omQualifiedId . cmOthers . cnvMembers
    const (Right $ pure remoteBob) === (fmap findRemote <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)
