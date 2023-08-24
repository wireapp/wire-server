module Testlib.Service where

import Prelude

data Service = Brig | Galley | Cannon | Gundeck | Cargohold | Nginz | Spar | BackgroundWorker | Stern | FederatorInternal
  deriving
    ( Show,
      Eq,
      Ord,
      Enum,
      Bounded
    )

serviceName :: Service -> String
serviceName = \case
  Brig -> "brig"
  Galley -> "galley"
  Cannon -> "cannon"
  Gundeck -> "gundeck"
  Cargohold -> "cargohold"
  Nginz -> "nginz"
  Spar -> "spar"
  BackgroundWorker -> "backgroundWorker"
  Stern -> "stern"
  FederatorInternal -> "federator"

-- | Converts the service name to kebab-case.
configName :: Service -> String
configName = \case
  Brig -> "brig"
  Galley -> "galley"
  Cannon -> "cannon"
  Gundeck -> "gundeck"
  Cargohold -> "cargohold"
  Nginz -> "nginz"
  Spar -> "spar"
  BackgroundWorker -> "background-worker"
  Stern -> "stern"
  FederatorInternal -> "federator"

data BackendName
  = BackendA
  | BackendB
  | -- | The index of dynamic backends begin with 1
    DynamicBackend Int
  deriving (Show, Eq, Ord)

allServices :: [Service]
allServices = [minBound .. maxBound]
