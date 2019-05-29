module Galley.Intra.Client
    ( lookupClients
    , notifyClientsAboutLegalHoldRequest
    , addLegalHoldClientToUser
    , getLegalHoldAuthToken
    ) where

import Imports
import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Brig.Types.Intra
import Brig.Types.User.Auth (SsoLogin(..))
import Brig.Types.Client.Prekey (LastPrekey, Prekey)
import Brig.Types.Team.LegalHold (LegalHoldClientRequest(..))
import Control.Monad.Catch
import Galley.App
import Galley.API.Error
import Galley.External.LegalHoldService
import Galley.Intra.Util
import Galley.Types (UserClients, filterClients)
import Data.Id
import Data.Text.Encoding
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

getLegalHoldAuthToken :: UserId -> Galley OpaqueAuthToken
getLegalHoldAuthToken uid = do
    (brigHost, brigPort) <- brigReq
    r <- call "brig" $
           method POST
            . host brigHost
            . port brigPort
            . path "/i/sso-login" -- ^ TODO: switch to '/i/legalhold-login'
            . json (SsoLogin uid (Just "auth_token"))
            . expect2xx
    case getCookie "auth_token" r of
        Nothing -> throwM internalError
        Just c -> pure . OpaqueAuthToken . decodeUtf8 $ cookie_value c

addLegalHoldClientToUser :: UserId -> [Prekey] -> Galley (Text, Text)
addLegalHoldClientToUser _uid _prekeys = undefined
