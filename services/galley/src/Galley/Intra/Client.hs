module Galley.Intra.Client
    ( lookupClients
    , notifyClientsAboutLegalHoldRequest
    ) where

import Imports
import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Brig.Types.Intra
import Brig.Types.Client.Prekey (LastPrekey, Prekey)
import Brig.Types.Team.LegalHold (LegalHoldClientRequest(..))
import Galley.App
import Galley.Intra.Util
import Galley.Types (UserClients, filterClients)
import Data.Id
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified Data.Set as Set

lookupClients :: [UserId] -> Galley UserClients
lookupClients uids = do
    (brigHost, brigPort) <- brigReq
    r <- call "brig"
        $ method POST . host brigHost . port brigPort
        . path "/i/clients"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
    clients <- parseResponse (Error status502 "server-error") r
    return $ filterClients (not . Set.null) clients

notifyClientsAboutLegalHoldRequest :: UserId -> UserId -> LastPrekey -> [Prekey] -> Galley ()
notifyClientsAboutLegalHoldRequest requesterUid targetUid lastPrekey' prekeys = do
    (brigHost, brigPort) <- brigReq
    void . call "brig"
                $ method POST
                . host brigHost
                . port brigPort
                . path "/i/clients/legalhold/request"
                . json (LegalHoldClientRequest requesterUid targetUid lastPrekey' prekeys)
                . expect2xx
