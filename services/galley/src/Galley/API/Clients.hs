module Galley.API.Clients
  ( getClientsH,
    addClientH,
    rmClientH,
  )
where

import Control.Lens (view)
import Data.Id
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Intra.Client as Intra
import Galley.Options
import Galley.Types.Clients (clientIds, fromUserClients)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities

getClientsH :: UserId -> Galley Response
getClientsH usr = do
  json <$> getClients usr

getClients :: UserId -> Galley [ClientId]
getClients usr = do
  isInternal <- view $ options . optSettings . setIntraListing
  clts <-
    if isInternal
      then fromUserClients <$> Intra.lookupClients [usr]
      else Data.lookupClients [usr]
  return $ clientIds usr clts

addClientH :: UserId ::: ClientId -> Galley Response
addClientH (usr ::: clt) = do
  Data.updateClient True usr clt
  return empty

rmClientH :: UserId ::: ClientId -> Galley Response
rmClientH (usr ::: clt) = do
  Data.updateClient False usr clt
  return empty
