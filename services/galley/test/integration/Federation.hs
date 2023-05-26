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

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

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

  -- Setup a conversation for a known remote domain.
  -- Include that domain in the old and new lists so
  -- if the function is acting up we know it will be
  -- working on the domain.
  -- updateFedDomainsTestNoop env remoteDomain interval

  -- Adding a new federation domain, this too should be a no-op
  -- updateFedDomainsAddRemote env remoteDomain remoteDomain2 interval

  -- Removing a single domain
  updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval

-- Removing multiple domains
-- updateFedDomainsCallback old new

constHandlers :: MonadIO m => [RetryStatus -> Handler m Bool]
constHandlers = [const $ Handler $ (\(_ :: SomeException) -> pure True)]

updateFedDomainRemoveRemoteFromLocal :: Env -> Domain -> Domain -> Int -> TestM ()
updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval = recovering x3 constHandlers $ const $ do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      old = FederationDomainConfigs AllowDynamic [FederationDomainConfig remoteDomain FullSearch, FederationDomainConfig remoteDomain2 FullSearch] interval
      new = old {remotes = [FederationDomainConfig remoteDomain2 FullSearch]}
  qalice <- randomQualifiedUser
  bobId <- randomId
  charlieId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
      remoteCharlie = Qualified charlieId remoteDomain2
  -- Create a conversation
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qConvId = Qualified convId localDomain
  connectWithRemoteUser alice remoteBob
  connectWithRemoteUser alice remoteCharlie
  _ <- postQualifiedMembers alice (remoteCharlie <| remoteBob :| []) qConvId
  liftIO $ threadDelay $ 3 * 1000000
  -- Remove the remote user from the local domain
  liftIO $ runApp env $ updateFedDomainsCallback old new
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
  liftIO $ runApp env $ updateFedDomainsCallback old new
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
  liftIO $ runApp env $ updateFedDomainsCallback old new
  -- Check that the conversation still exists.
  getConvQualified (qUnqualified qalice) (Qualified convId localDomain) !!! do
    const 200 === statusCode
    let findRemote :: Conversation -> Maybe (Qualified UserId)
        findRemote = find (== remoteBob) . fmap omQualifiedId . cmOthers . cnvMembers
    const (Right $ pure remoteBob) === (fmap findRemote <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)
