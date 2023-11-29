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

module Brig.Effects.FederationConfigStore.Cassandra
  ( interpretFederationDomainConfig,
    AddFederationRemoteResult (..),
  )
where

import Brig.Data.Instances ()
import Brig.Effects.FederationConfigStore
import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens
import Control.Monad.Catch (throwM)
import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Database.CQL.Protocol (SerialConsistency (LocalSerialConsistency), serialConsistency)
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search

interpretFederationDomainConfig ::
  forall m r a.
  ( MonadClient m,
    Member (Embed m) r
  ) =>
  Maybe FederationStrategy ->
  [FederationDomainConfig] ->
  Sem (FederationConfigStore ': r) a ->
  Sem r a
interpretFederationDomainConfig mFedStrategy cfgs =
  interpret $
    embed @m . \case
      GetFederationConfig d -> getFederationConfig' cfgs d
      GetFederationConfigs -> getFederationConfigs' mFedStrategy cfgs
      AddFederationConfig cnf -> addFederationConfig' cfgs cnf
      UpdateFederationConfig d cnf -> updateFederationConfig' cfgs d cnf
      AddFederationRemoteTeam d t -> addFederationRemoteTeam' cfgs d t
      RemoveFederationRemoteTeam d t -> removeFederationRemoteTeam' d t
      GetFederationRemoteTeams d -> getFederationRemoteTeams' d

-- | Compile config file list into a map indexed by domains.  Use this to make sure the config
-- file is consistent (ie., no two entries for the same domain).
remotesMapFromCfgFile :: (Monad m) => [FederationDomainConfig] -> m (Map Domain FederationDomainConfig)
remotesMapFromCfgFile cfg = do
  let dict = [(cnf.domain, cnf) | cnf <- cfg]
      merge c c' =
        if c == c'
          then c
          else error $ "error in config file: conflicting parameters on domain: " <> show (c, c')
  pure $ Map.fromListWith merge dict

-- | Return the config file list.  Use this to make sure the config file is consistent (ie.,
-- no two entries for the same domain).  Based on `remotesMapFromCfgFile`.
remotesListFromCfgFile :: Monad m => [FederationDomainConfig] -> m [FederationDomainConfig]
remotesListFromCfgFile cfgs = Map.elems <$> remotesMapFromCfgFile cfgs

getFederationConfigs' :: forall m. (MonadClient m) => Maybe FederationStrategy -> [FederationDomainConfig] -> m FederationDomainConfigs
getFederationConfigs' mFedStrategy cfgs = do
  -- FUTUREWORK: we should solely rely on `db` in the future for remote domains; merging
  -- remote domains from `cfg` is just for providing an easier, more robust migration path.
  -- See
  -- https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections,
  -- http://docs.wire.com/developer/developer/federation-design-aspects.html#configuring-remote-connections-dev-perspective
  remotes <-
    (<>)
      <$> getFederationRemotes
      <*> remotesListFromCfgFile cfgs

  defFederationDomainConfigs
    & maybe id (\v cfg -> cfg {strategy = v}) mFedStrategy
    & (\cfg -> cfg {remotes = remotes})
    & pure

maxKnownNodes :: Int
maxKnownNodes = 10000

getFederationConfig' :: MonadClient m => [FederationDomainConfig] -> Domain -> m (Maybe FederationDomainConfig)
getFederationConfig' cfgs rDomain = case find ((== rDomain) . domain) cfgs of
  Just cfg -> pure . Just $ cfg -- the configuration from the file has precedence
  Nothing -> do
    mCnf <- retry x1 (query1 q (params LocalQuorum (Identity rDomain)))
    case mCnf of
      Just (p, r) -> Just . FederationDomainConfig rDomain p <$> toRestriction rDomain r
      Nothing -> pure Nothing
  where
    q :: PrepQuery R (Identity Domain) (FederatedUserSearchPolicy, Int32)
    q = "SELECT search_policy, restriction FROM federation_remotes WHERE domain = ?"

getFederationRemotes :: forall m. MonadClient m => m [FederationDomainConfig]
getFederationRemotes = (\(d, p, r) -> FederationDomainConfig d p r) <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy, FederationRestriction)]
    qry = do
      res <- retry x1 . query get $ params LocalQuorum ()
      forM res $ \(d, p, rInt) -> do
        (d,p,) <$> toRestriction d rInt

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy, Int32)
    get = fromString $ "SELECT domain, search_policy, restriction FROM federation_remotes LIMIT " <> show maxKnownNodes

addFederationConfig' :: MonadClient m => [FederationDomainConfig] -> FederationDomainConfig -> m AddFederationRemoteResult
addFederationConfig' cfgs (FederationDomainConfig rDomain searchPolicy restriction) = do
  cfg <- remotesMapFromCfgFile cfgs
  conflict <- domainExistsInConfig cfg (FederationDomainConfig rDomain searchPolicy restriction)
  if conflict
    then pure $ AddFederationRemoteDivergingConfig cfg
    else do
      l <- length <$> getFederationRemotes
      if l >= maxKnownNodes
        then pure AddFederationRemoteMaxRemotesReached
        else
          AddFederationRemoteSuccess <$ do
            retry x5 (write addConfig (params LocalQuorum (rDomain, searchPolicy, fromRestriction restriction)))
            case restriction of
              FederationRestrictionByTeam tids ->
                retry x5 . batch . forM_ tids $ addPrepQuery addTeams . (rDomain,)
              FederationRestrictionAllowAll -> pure ()
  where
    -- If remote domain is registered in config file, the version that can be added to the
    -- database must be the same.
    domainExistsInConfig :: (Monad m) => (Map Domain FederationDomainConfig) -> FederationDomainConfig -> m Bool
    domainExistsInConfig cfg fedDomConf = do
      pure $ case Map.lookup (domain fedDomConf) cfg of
        Nothing -> False
        Just fedDomConf' -> fedDomConf' /= fedDomConf

    addConfig :: PrepQuery W (Domain, FederatedUserSearchPolicy, Int32) ()
    addConfig = "INSERT INTO federation_remotes (domain, search_policy, restriction) VALUES (?, ?, ?)"

    addTeams :: PrepQuery W (Domain, TeamId) ()
    addTeams = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

updateFederationConfig' :: MonadClient m => [FederationDomainConfig] -> Domain -> FederationDomainConfig -> m UpdateFederationResult
updateFederationConfig' cfgs dom (FederationDomainConfig rDomain searchPolicy restriction) = do
  if dom /= rDomain
    then pure UpdateFederationRemoteDomainMismatch
    else
      if dom `elem` (domain <$> cfgs)
        then pure UpdateFederationRemoteDivergingConfig
        else do
          let configParams =
                ( params
                    LocalQuorum
                    (searchPolicy, fromRestriction restriction, rDomain)
                )
                  { serialConsistency = Just LocalSerialConsistency
                  }
          r <- retry x1 (trans updateConfig configParams)
          updateTeams
          case r of
            [] -> pure UpdateFederationRemoteNotFound
            [_] -> pure UpdateFederationSuccess
            _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    updateConfig :: PrepQuery W (FederatedUserSearchPolicy, Int32, Domain) x
    updateConfig = "UPDATE federation_remotes SET search_policy = ?, restriction = ? WHERE domain = ? IF EXISTS"

    updateTeams :: MonadClient m => m ()
    updateTeams = retry x5 $ do
      write dropTeams (params LocalQuorum (Identity rDomain))
      case restriction of
        FederationRestrictionByTeam tids ->
          batch . forM_ tids $ addPrepQuery insertTeam . (rDomain,)
        FederationRestrictionAllowAll -> pure ()

    dropTeams :: PrepQuery W (Identity Domain) ()
    dropTeams = "DELETE FROM federation_remote_teams WHERE domain = ?"

    insertTeam :: PrepQuery W (Domain, TeamId) ()
    insertTeam = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

addFederationRemoteTeam' :: MonadClient m => [FederationDomainConfig] -> Domain -> TeamId -> m AddFederationRemoteTeamResult
addFederationRemoteTeam' cfgs rDomain tid = do
  mDom <- getFederationConfig' cfgs rDomain
  case mDom of
    Nothing ->
      pure AddFederationRemoteTeamDomainNotFound
    Just (FederationDomainConfig _ _ FederationRestrictionAllowAll) ->
      pure AddFederationRemoteTeamRestrictionAllowAll
    Just _ -> do
      retry x1 $ write add (params LocalQuorum (rDomain, tid))
      pure AddFederationRemoteTeamSuccess
  where
    add :: PrepQuery W (Domain, TeamId) ()
    add = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

getFederationRemoteTeams' :: MonadClient m => Domain -> m [FederationRemoteTeam]
getFederationRemoteTeams' rDomain = do
  fmap (FederationRemoteTeam . runIdentity) <$> retry x1 (query get (params LocalQuorum (Identity rDomain)))
  where
    get :: PrepQuery R (Identity Domain) (Identity TeamId)
    get = "SELECT team FROM federation_remote_teams WHERE domain = ?"

removeFederationRemoteTeam' :: MonadClient m => Domain -> TeamId -> m ()
removeFederationRemoteTeam' rDomain rteam =
  retry x1 $ write delete (params LocalQuorum (rDomain, rteam))
  where
    delete :: PrepQuery W (Domain, TeamId) ()
    delete = "DELETE FROM federation_remote_teams WHERE domain = ? AND team = ?"

data RestrictionException = RestrictionException Int32

instance Show RestrictionException where
  show (RestrictionException v) =
    "Expected a RestrictionPolicy encoding, but found a value " <> show v

instance Exception RestrictionException

toRestriction :: MonadClient m => Domain -> Int32 -> m FederationRestriction
toRestriction _ 0 = pure FederationRestrictionAllowAll
toRestriction dom 1 =
  fmap FederationRestrictionByTeam $
    runIdentity <$$> retry x1 (query getTeams (params LocalQuorum (Identity dom)))
  where
    getTeams :: PrepQuery R (Identity Domain) (Identity TeamId)
    getTeams = fromString $ "SELECT team FROM federation_remote_teams WHERE domain = ?"
toRestriction _ v = throwM . RestrictionException $ v

fromRestriction :: FederationRestriction -> Int32
fromRestriction FederationRestrictionAllowAll = 0
fromRestriction (FederationRestrictionByTeam _) = 1
