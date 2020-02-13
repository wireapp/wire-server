module Galley.API.CustomBackend
  ( getCustomBackendByDomain,
    internalPutCustomBackendByDomain,
    internalDeleteCustomBackendByDomain,
  )
where

import Control.Monad.Catch
import Galley.API.Error
import Galley.API.Util
import Galley.App
import qualified Galley.Data.CustomBackend as Data
import Galley.Types
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities

-- PUBLIC ---------------------------------------------------------------------

getCustomBackendByDomain :: EmailDomain ::: JSON -> Galley Response
getCustomBackendByDomain (domain ::: _) =
  Data.getCustomBackend domain >>= \case
    Nothing -> throwM (customBackendNotFound domain)
    Just customBackend -> pure (json customBackend)

-- INTERNAL -------------------------------------------------------------------

internalPutCustomBackendByDomain :: EmailDomain ::: JsonRequest CustomBackend -> Galley Response
internalPutCustomBackendByDomain (domain ::: req) = do
  customBackend <- fromJsonBody req
  Data.setCustomBackend domain customBackend
  pure (empty & setStatus status201)

internalDeleteCustomBackendByDomain :: EmailDomain ::: JSON -> Galley Response
internalDeleteCustomBackendByDomain (domain ::: _) = do
  Data.deleteCustomBackend domain
  pure (empty & setStatus status200)
