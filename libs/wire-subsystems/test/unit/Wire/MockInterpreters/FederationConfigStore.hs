module Wire.MockInterpreters.FederationConfigStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.API.Routes.FederationDomainConfig
import Wire.FederationConfigStore

inMemoryFederationConfigStoreInterpreter ::
  (Member (State [FederationDomainConfig]) r) =>
  InterpreterFor FederationConfigStore r
inMemoryFederationConfigStoreInterpreter =
  interpret $ \case
    GetFederationConfig domain -> gets $ find (\cfg -> cfg.domain == domain)
    GetFederationConfigs -> do
      remoteConfigs <- get
      pure $ FederationDomainConfigs AllowDynamic remoteConfigs 1
    AddFederationConfig newCfg -> do
      modify $ (newCfg :) . deleteBy (\a b -> a.domain == b.domain) newCfg
      pure AddFederationRemoteSuccess
    UpdateFederationConfig _ ->
      error "UpdateFederationConfig not implemented in inMemoryFederationConfigStoreInterpreter"
    AddFederationRemoteTeam _ _ ->
      error "AddFederationRemoteTeam not implemented in inMemoryFederationConfigStoreInterpreter"
    RemoveFederationRemoteTeam _ _ ->
      error "RemoveFederationRemoteTeam not implemented in inMemoryFederationConfigStoreInterpreter"
    GetFederationRemoteTeams _ ->
      error "GetFederationRemoteTeams not implemented in inMemoryFederationConfigStoreInterpreter"
    BackendFederatesWith _ ->
      error "BackendFederatesWith not implemented in inMemoryFederationConfigStoreInterpreter"

runFederationConfigStoreInMemory :: InterpreterFor FederationConfigStore r
runFederationConfigStoreInMemory =
  evalState []
    . inMemoryFederationConfigStoreInterpreter
    . raiseUnder
