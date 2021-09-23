module Spar.Sem.ScimUserTimesStore where

import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis)
import Imports
import Polysemy
import Web.Scim.Schema.Common (WithId)
import Web.Scim.Schema.Meta (WithMeta)

data ScimUserTimesStore m a where
  Write :: WithMeta (WithId UserId a) -> ScimUserTimesStore m ()
  Read :: UserId -> ScimUserTimesStore m (Maybe (UTCTimeMillis, UTCTimeMillis))
  Delete :: UserId -> ScimUserTimesStore m ()

makeSem ''ScimUserTimesStore
