module Wire.MockInterpreters.SparAPIAccess where

import Imports
import Polysemy
import Wire.SparAPIAccess

-- | interprets galley by statically returning the values passed
miniSparAPIAccess :: InterpreterFor SparAPIAccess r
miniSparAPIAccess = interpret $ \case
  GetIdentityProviders _ -> error "GetIdentityProviders  not implemented in miniSparAPIAccess"
