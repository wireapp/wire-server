module Wire.MockInterpreters.SparAPIAccess where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.User.IdentityProvider
import Wire.SparAPIAccess

-- | interprets galley by statically returning the values passed
miniSparAPIAccess :: (Member (Input (Map TeamId IdPList)) r) => InterpreterFor SparAPIAccess r
miniSparAPIAccess = interpret $ \case
  GetIdentityProviders tid ->
    Map.findWithDefault (IdPList []) tid <$> input

emptySparAPIAccess :: InterpreterFor SparAPIAccess r
emptySparAPIAccess = runInputConst mempty . miniSparAPIAccess . raiseUnder
