module Spar.Sem.ScimTokenStore where

import Imports
import Polysemy
import Data.Id
import Wire.API.User.Scim

data ScimTokenStore m a where
  Insert :: ScimToken -> ScimTokenInfo -> ScimTokenStore m ()
  Lookup :: ScimToken -> ScimTokenStore m (Maybe ScimTokenInfo)
  GetByTeam :: TeamId -> ScimTokenStore m [ScimTokenInfo]
  Delete :: TeamId -> ScimTokenId -> ScimTokenStore m ()

makeSem ''ScimTokenStore

