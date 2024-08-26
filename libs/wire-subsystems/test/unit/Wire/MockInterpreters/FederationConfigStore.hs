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
    UpdateFederationConfig cfg -> undefined cfg
    AddFederationRemoteTeam domain team -> undefined domain team
    RemoveFederationRemoteTeam domain team -> undefined domain team
    GetFederationRemoteTeams domain -> undefined domain
    BackendFederatesWith remoteMaybeTeam -> undefined remoteMaybeTeam

runFederationConfigStoreInMemory :: InterpreterFor FederationConfigStore r
runFederationConfigStoreInMemory =
  evalState []
    . inMemoryFederationConfigStoreInterpreter
    . raiseUnder
