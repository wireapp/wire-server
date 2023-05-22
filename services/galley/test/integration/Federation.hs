module Federation where
import TestSetup
import Control.Lens ((^.))
import Imports
import Galley.Run
import Control.Monad.Codensity (lowerCodensity)
import Wire.API.Routes.FederationDomainConfig
import Data.Domain
import Wire.API.User.Search
import API.Util
import Data.Id
import Data.List.NonEmpty
import Data.Qualified
import Wire.API.Conversation
import Bilge.Assert
import Bilge.Response
import Galley.Options (optSettings, setFederationDomain)
import Galley.Env

updateFedDomainsTest :: TestM ()
updateFedDomainsTest = do
  s <- ask
  let opts = s ^. tsGConf
  -- Don't need the actual server, and we certainly don't want it running.
  (_, env) <- liftIO $ lowerCodensity $ mkApp opts
  -- Common variables.
  let interval = (maxBound :: Int) `div` 2 -- Very large values so that we don't have to worry about automatic updates
      remoteDomain  = Domain "far-away.example.com"
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
  -- liftIO $ updateFedDomainsCallback env old new

updateFedDomainRemoveRemoteFromLocal :: Env -> Domain -> Domain -> Int -> TestM ()
updateFedDomainRemoveRemoteFromLocal env remoteDomain remoteDomain2 interval = recovering x3 [const . Handler $ pure . _] $ const $ do
  s <- ask
  let opts = s ^. tsGConf
      localDomain = opts ^. optSettings . setFederationDomain
      old = FederationDomainConfigs AllowList [FederationDomainConfig remoteDomain FullSearch, FederationDomainConfig remoteDomain2 FullSearch] interval
      new = old { remotes = [FederationDomainConfig remoteDomain2 FullSearch] }
  qalice <- randomQualifiedUser
  bobId <- randomId
  charlieId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
      remoteCharlie = Qualified charlieId remoteDomain2
  -- Create a conversation
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  connectWithRemoteUser alice remoteBob
  connectWithRemoteUser alice remoteCharlie
  _ <- postQualifiedMembers alice (remoteCharlie <| remoteBob :| []) convId
  liftIO $ threadDelay $ 10000
  -- Remove the remote user from the local domain
  liftIO $ updateFedDomainsCallback env old new
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
      old = FederationDomainConfigs AllowList [FederationDomainConfig remoteDomain FullSearch] interval
      new = old { remotes = FederationDomainConfig remoteDomain2 FullSearch : remotes old }
  qalice <- randomQualifiedUser
  bobId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
  -- Create a conversation
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  connectWithRemoteUser alice remoteBob
  _ <- postQualifiedMembers alice (remoteBob :| []) convId

  -- No-op
  liftIO $ updateFedDomainsCallback env old new
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
      old = FederationDomainConfigs AllowList [FederationDomainConfig remoteDomain FullSearch] interval
      new = old
  qalice <- randomQualifiedUser
  bobId <- randomId
  let alice = qUnqualified qalice
      remoteBob = Qualified bobId remoteDomain
  -- Create a conversation
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  connectWithRemoteUser alice remoteBob
  _ <- postQualifiedMembers alice (remoteBob :| []) convId
  -- No-op
  liftIO $ updateFedDomainsCallback env old new
  -- Check that the conversation still exists.
  getConvQualified (qUnqualified qalice) (Qualified convId localDomain) !!! do
    const 200 === statusCode
    let findRemote :: Conversation -> Maybe (Qualified UserId)
        findRemote = find (== remoteBob) . fmap omQualifiedId . cmOthers . cnvMembers
    const (Right $ pure remoteBob) === (fmap findRemote <$> responseJsonEither)
    const (Right qalice) === (fmap (memId . cmSelf . cnvMembers) <$> responseJsonEither)