module Galley.Intra.Client
    ( lookupClients
    , notifyClientsAboutLegalHoldRequest
    , addLegalHoldClientToUser
    , removeLegalHoldClientFromUser
    , getLegalHoldAuthToken
    ) where

import Imports
import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Brig.Types.Intra
import Brig.Types.User.Auth (LegalHoldLogin(..))
import Brig.Types.Client.Prekey (LastPrekey, Prekey)
import Brig.Types.Client
import Brig.Types.Team.LegalHold (LegalHoldClientRequest(..))
import Data.ByteString.Conversion (toByteString')
import Control.Monad.Catch
import Galley.App
import Galley.API.Error
import Galley.External.LegalHoldService
import Galley.Intra.Util
import Galley.Types (UserClients, filterClients)
import Data.Id
import Data.Text.Encoding
import Data.Misc
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified System.Logger.Class as Logger
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

notifyClientsAboutLegalHoldRequest :: UserId -> UserId -> LastPrekey -> Galley ()
notifyClientsAboutLegalHoldRequest requesterUid targetUid lastPrekey' = do
    (brigHost, brigPort) <- brigReq
    void . call "brig"
                $ method POST
                . host brigHost
                . port brigPort
                . paths ["i", "clients", "legalhold", toByteString' targetUid, "request"]
                . json (LegalHoldClientRequest requesterUid lastPrekey')
                . expect2xx

getLegalHoldAuthToken :: UserId -> Maybe (PlainTextPassword "visible") -> Galley OpaqueAuthToken
getLegalHoldAuthToken uid pw = do
    (brigHost, brigPort) <- brigReq
    r <- call "brig" $
           method POST
            . host brigHost
            . port brigPort
            . path "/i/legalhold-login"
            . queryItem "persist" "true"
            . json (LegalHoldLogin uid pw Nothing)
            . expect2xx
    case getCookieValue "zuid" r of
        Nothing -> do
            Logger.warn $ Logger.msg @Text "Response from login missing auth cookie"
            throwM internalError
        Just c -> pure . OpaqueAuthToken . decodeUtf8 $ c

addLegalHoldClientToUser :: UserId -> ConnId -> [Prekey] -> LastPrekey -> Galley ClientId
addLegalHoldClientToUser uid connId prekeys lastPrekey' = do
    clientId <$> brigAddClient uid connId lhClient
  where
    lhClient =
        NewClient prekeys
                  lastPrekey'
                  LegalHoldClientType
                  Nothing
                  (Just LegalHoldClient)
                  Nothing
                  Nothing
                  Nothing

removeLegalHoldClientFromUser :: UserId -> Galley ()
removeLegalHoldClientFromUser targetUid = do
    (brigHost, brigPort) <- brigReq
    void . call "brig"
        $ method DELETE
        . host brigHost
        . port brigPort
                . paths ["i", "clients", "legalhold", toByteString' targetUid]
        . contentJson
        . expect2xx

brigAddClient :: UserId -> ConnId -> NewClient "visible" -> Galley Client
brigAddClient uid connId client = do
    (brigHost, brigPort) <- brigReq
    r <- call "brig"
        $ method POST
        . host brigHost
        . port brigPort
        . header "Z-Connection" (toByteString' connId)
        . paths ["i", "clients", toByteString' uid]
        . contentJson
        . json client
        . expect2xx
    parseResponse (Error status502 "server-error") r
