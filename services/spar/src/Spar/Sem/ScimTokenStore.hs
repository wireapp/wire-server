module Spar.Sem.ScimTokenStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.User.Scim

data ScimTokenStore m a where
  Insert :: ScimToken -> ScimTokenInfo -> ScimTokenStore m ()
  Lookup :: ScimToken -> ScimTokenStore m (Maybe ScimTokenInfo)
  GetByTeam :: TeamId -> ScimTokenStore m [ScimTokenInfo]
  Delete :: TeamId -> ScimTokenId -> ScimTokenStore m ()
  DeleteByTeam :: TeamId -> ScimTokenStore m ()

makeSem ''ScimTokenStore
