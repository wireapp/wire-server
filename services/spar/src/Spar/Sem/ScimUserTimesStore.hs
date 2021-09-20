module Spar.Sem.ScimUserTimesStore where

import Polysemy
import Imports
import Web.Scim.Schema.Meta (WithMeta)
import Web.Scim.Schema.Common (WithId)
import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis)

data ScimUserTimesStore m a where
    Write :: WithMeta (WithId UserId a) -> ScimUserTimesStore m ()
    Read :: UserId -> ScimUserTimesStore m (Maybe (UTCTimeMillis, UTCTimeMillis))
    Delete :: UserId -> ScimUserTimesStore m ()

makeSem ''ScimUserTimesStore

