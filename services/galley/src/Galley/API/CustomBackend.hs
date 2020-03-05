module Galley.API.CustomBackend
  ( getCustomBackendByDomainH,
    internalPutCustomBackendByDomainH,
    internalDeleteCustomBackendByDomainH,
  )
where

import Control.Monad.Catch
import Data.Domain (Domain)
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

getCustomBackendByDomainH :: Domain ::: JSON -> Galley Response
getCustomBackendByDomainH (domain ::: _) =
  json <$> getCustomBackendByDomain domain

getCustomBackendByDomain :: Domain -> Galley CustomBackend
getCustomBackendByDomain domain =
  Data.getCustomBackend domain >>= \case
    Nothing -> throwM (customBackendNotFound domain)
    Just customBackend -> pure customBackend

-- INTERNAL -------------------------------------------------------------------

internalPutCustomBackendByDomainH :: Domain ::: JsonRequest CustomBackend -> Galley Response
internalPutCustomBackendByDomainH (domain ::: req) = do
  customBackend <- fromJsonBody req
  -- simple enough to not need a separate function
  Data.setCustomBackend domain customBackend
  pure (empty & setStatus status201)

internalDeleteCustomBackendByDomainH :: Domain ::: JSON -> Galley Response
internalDeleteCustomBackendByDomainH (domain ::: _) = do
  Data.deleteCustomBackend domain
  pure (empty & setStatus status200)
