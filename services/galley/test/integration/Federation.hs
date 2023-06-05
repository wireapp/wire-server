module Federation where

import API.Util
import Bilge.Assert
import Bilge.Response
import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.Codensity (lowerCodensity)
import Data.Domain
import Data.Id
import Data.List.NonEmpty
import Data.Qualified
import Galley.Env
import Galley.Monad
import Galley.Options
import Galley.Run
import Imports
import TestSetup
import UnliftIO.Retry
import Wire.API.Conversation
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import qualified Data.Set as Set
import Test.Tasty.HUnit
import Galley.API.Util
import qualified Data.UUID as UUID
import qualified Galley.Data.Conversation.Types as Types
import Galley.Types.Conversations.Members (defMemberStatus, LocalMember (..), RemoteMember (..))
import Wire.API.Conversation.Role (roleNameWireMember)
import Wire.API.Conversation.Protocol (Protocol(..))

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
        lconv = toLocalUnsafe localDomain $ Types.Conversation
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
      remoteDomain = Domain "far-away.example.org"
      remoteDomain2 = Domain "far-away-two.example.net"
  liftIO $ assertBool "remoteDomain is different to local domain" $ remoteDomain /= opts ^. optSettings . setFederationDomain
  liftIO $ assertBool "remoteDomain2 is different to local domain" $ remoteDomain2 /= opts ^. optSettings . setFederationDomain
  -- Setup a conversation for a known remote domain.
  -- Include that domain in the old and new lists so
  -- if the function is acting up we know it will be
  -- working on the domain.
  updateFedDomainsTestNoop env remoteDomain interval

  -- Adding a new federation domain, this too should be a no-op
  updateFedDomainsAddRemote env remoteDomain remoteDomain2 interval

  -- Removing a single domain
  updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval

-- Removing multiple domains
-- updateFedDomainsCallback old new

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
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      new = FederationDomainConfigs AllowDynamic [FederationDomainConfig remoteDomain2 FullSearch] interval
      old = new {remotes = FederationDomainConfig remoteDomain FullSearch : remotes new}
  qalice <- randomQualifiedUser
  bobId <- randomId
  charlieId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
      remoteCharlie = Qualified charlieId remoteDomain2
  -- Create a conversation
  conv <- postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qConvId = decodeQualifiedConvId conv
      convId = qUnqualified qConvId
  connectWithRemoteUser alice remoteBob
  connectWithRemoteUser alice remoteCharlie
  _ <- postQualifiedMembers alice (remoteCharlie <| remoteBob :| []) qConvId
  -- Remove the remote user from the local domain
  liftIO $ runApp env $ deleteFederationDomains old new
  -- Check that the conversation still exists.
  getConvQualified (qUnqualified qalice) (Qualified convId localDomain) !!! do
    const 200 === statusCode
    let findRemote :: Qualified UserId -> Conversation -> Maybe (Qualified UserId)
        findRemote u = find (== u) . fmap omQualifiedId . cmOthers . cnvMembers
    -- Check that only one remote user was removed.
    const (Right Nothing) === (fmap (findRemote remoteBob) <$> responseJsonEither)
    const (Right $ pure remoteCharlie) === (fmap (findRemote remoteCharlie) <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)

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
