-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationConfigSubsystem.Interpreter where

import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig
import Wire.FederationConfigStore qualified as Store
import Wire.FederationConfigSubsystem

-- | Interpreter for federation config store accesses.
-- The config file is injected into the interpreter and has precedence over the database.
-- The config file is static and can only be changed by restarting the service.
-- If a domain is configured in the config file, it is not allowed to add it to the database.
-- If a domain is configured in the config file, it is not allowed to update it in the database.
-- If a domain is configured in the config file, it is not allowed to add a team restriction to it in the database.
-- In the future the config file will be removed and the database will be the only source of truth.
interpretFederationConfigSubsystem ::
  forall r a.
  (Member Store.FederationConfigStore r) =>
  Maybe FederationStrategy ->
  Map Domain FederationDomainConfig ->
  Sem (FederationConfigSubsystem ': r) a ->
  Sem r a
interpretFederationConfigSubsystem mFedStrategy fedCfgs =
  interpret $
    \case
      GetFederationConfig d -> getFederationConfigImpl fedCfgs d
      GetFederationConfigs -> getFederationConfigsImpl mFedStrategy fedCfgs
      AddFederationConfig cnf -> addFederationConfigImpl fedCfgs cnf
      UpdateFederationConfig cnf -> updateFederationConfigImpl fedCfgs cnf
      AddFederationRemoteTeam d t -> addFederationRemoteTeamImpl fedCfgs d t
      RemoveFederationRemoteTeam d t -> removeFederationRemoteTeamImpl d t
      GetFederationRemoteTeams d -> getFederationRemoteTeamsImpl d
      BackendFederatesWith mtid -> backendFederatesWithImpl mtid fedCfgs mFedStrategy

getFederationConfigImpl :: (Member Store.FederationConfigStore r) => Map Domain FederationDomainConfig -> Domain -> Sem r (Maybe FederationDomainConfig)
getFederationConfigImpl cfgs rDomain =
  case Map.lookup rDomain cfgs of
    -- the configuration from the file has precedence (if exists there should not be a db entry at all)
    Just cfg -> pure . Just $ cfg
    Nothing -> Store.getFederationConfig rDomain

getFederationConfigsImpl ::
  (Member Store.FederationConfigStore r) =>
  Maybe FederationStrategy ->
  Map Domain FederationDomainConfig ->
  Sem r FederationDomainConfigs
getFederationConfigsImpl mFedStrategy cfgs = do
  -- FUTUREWORK: we should solely rely on `db` in the future for remote domains; merging
  -- remote domains from `cfg` is just for providing an easier, more robust migration path.
  -- See
  -- https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections,
  -- http://docs.wire.com/developer/developer/federation-design-aspects.html#configuring-remote-connections-dev-perspective
  -- (because the creation and update of a federation config is guarded, we can safely merge the two configs here)
  remotes <-
    (<>)
      <$> Store.getFederationConfigs
      <*> pure (Map.elems cfgs)

  defFederationDomainConfigs
    & maybe id (\v cfg -> cfg {strategy = v}) mFedStrategy
    & (\cfg -> cfg {remotes = remotes})
    & pure

addFederationConfigImpl :: (Member Store.FederationConfigStore r) => Map Domain FederationDomainConfig -> FederationDomainConfig -> Sem r Store.AddFederationRemoteResult
addFederationConfigImpl cfg fedCfg = do
  -- If a domain already exists in a config with a differing value, we do not allow adding it to the database
  if divergingDomainExistsInConfig fedCfg
    then
      pure $ Store.AddFederationRemoteDivergingConfig cfg
    else
      Store.addFederationConfig fedCfg
  where
    -- If remote domain is registered in config file, the version that can be added to the
    -- database must be the same.
    divergingDomainExistsInConfig :: FederationDomainConfig -> Bool
    divergingDomainExistsInConfig fedDomConf =
      case Map.lookup (domain fedDomConf) cfg of
        Nothing -> False
        Just fedDomConf' -> fedDomConf' /= fedDomConf

updateFederationConfigImpl :: (Member Store.FederationConfigStore r) => Map Domain FederationDomainConfig -> FederationDomainConfig -> Sem r Store.UpdateFederationResult
updateFederationConfigImpl cfgs fedCfg@(FederationDomainConfig rDomain _ _) = do
  -- if a domain already exists in a config, we do not allow update it
  if Map.member rDomain cfgs
    then pure Store.UpdateFederationRemoteDivergingConfig
    else do
      Store.updateFederationConfig fedCfg

addFederationRemoteTeamImpl :: (Member Store.FederationConfigStore r) => Map Domain FederationDomainConfig -> Domain -> TeamId -> Sem r Store.AddFederationRemoteTeamResult
addFederationRemoteTeamImpl cfgs rDomain tid = do
  mDom <- getFederationConfigImpl cfgs rDomain
  case mDom of
    Nothing ->
      pure Store.AddFederationRemoteTeamDomainNotFound
    Just (FederationDomainConfig _ _ FederationRestrictionAllowAll) ->
      pure Store.AddFederationRemoteTeamRestrictionAllowAll
    Just _ ->
      Store.addFederationRemoteTeam rDomain tid

removeFederationRemoteTeamImpl :: (Member Store.FederationConfigStore r) => Domain -> TeamId -> Sem r ()
removeFederationRemoteTeamImpl = Store.removeFederationRemoteTeam

getFederationRemoteTeamsImpl :: (Member Store.FederationConfigStore r) => Domain -> Sem r [FederationRemoteTeam]
getFederationRemoteTeamsImpl = Store.getFederationRemoteTeams

backendFederatesWithImpl :: (Member Store.FederationConfigStore r) => Remote (Maybe TeamId) -> Map Domain FederationDomainConfig -> Maybe FederationStrategy -> Sem r Bool
backendFederatesWithImpl (tUntagged -> Qualified rTeamM rDomain) staticCfgs = \case
  Nothing -> pure False
  Just AllowNone -> pure False
  Just AllowAll -> pure True
  Just AllowDynamic -> do
    getFederationConfigImpl staticCfgs rDomain >>= \case
      Nothing -> pure False
      Just fedDomainConfig ->
        case rTeamM of
          Nothing ->
            pure $ restriction fedDomainConfig == FederationRestrictionAllowAll
          Just rTeam ->
            case restriction fedDomainConfig of
              FederationRestrictionAllowAll ->
                pure True
              FederationRestrictionByTeam ts ->
                pure $ rTeam `elem` ts
