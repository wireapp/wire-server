module Spar.Sem.ScimExternalIdStore where

import Data.Id (TeamId, UserId)
import Imports
import Polysemy
import Polysemy.Check (deriveGenericK)
import Wire.API.User.Identity (Email)

data ScimExternalIdStore m a where
  Insert :: TeamId -> Email -> UserId -> ScimExternalIdStore m ()
  Lookup :: TeamId -> Email -> ScimExternalIdStore m (Maybe UserId)
  Delete :: TeamId -> Email -> ScimExternalIdStore m ()

deriving instance Show (ScimExternalIdStore m a)

makeSem ''ScimExternalIdStore
deriveGenericK ''ScimExternalIdStore
