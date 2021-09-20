module Spar.Sem.ScimExternalIdStore where

import Polysemy
import Imports
import Data.Id (UserId, TeamId)
import Wire.API.User.Identity (Email)

data ScimExternalIdStore m a where
    Insert :: TeamId -> Email -> UserId -> ScimExternalIdStore m ()
    Lookup :: TeamId -> Email -> ScimExternalIdStore m (Maybe UserId)
    Delete :: TeamId -> Email -> ScimExternalIdStore m ()

makeSem ''ScimExternalIdStore


