{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.User
    ( getConnections
    , deleteBot
    , reAuthUser
    , deleteUser
    , getContactList
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Brig.Types.Connection (UserIds (..))
import Bilge.RPC
import Brig.Types.Intra (ConnectionStatus (..), ReAuthUser (..))
import Brig.Types.Connection (Relation (..))
import Galley.App
import Galley.Intra.Util
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Data.ByteString.Char8 (pack, intercalate)
import Data.ByteString.Conversion
import Data.Char (toLower)
import Data.Id
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified Network.HTTP.Client.Internal as Http

getConnections :: UserId -> [UserId] -> Maybe Relation -> Galley [ConnectionStatus]
getConnections u uids rlt = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method GET . host h . port p
        . path "/i/users/connections-status"
        . queryItem "users" users
        . maybe id rfilter rlt
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    users   = intercalate "," $ toByteString' <$> u:uids
    rfilter = queryItem "filter" . (pack . map toLower . show)

deleteBot :: ConvId -> BotId -> Galley ()
deleteBot cid bot = do
    (h, p) <- brigReq
    void $ call "brig"
        $ method DELETE . host h . port p
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

reAuthUser :: UserId -> ReAuthUser -> Galley Bool
reAuthUser uid auth = do
    (h, p) <- brigReq
    let req = method GET . host h . port p
            . paths ["/i/users", toByteString' uid, "reauthenticate"]
            . json auth
    st <- statusCode . responseStatus <$> call "brig" (check . req)
    return $ if st == 200 then True
                          else False
  where
    check :: Request -> Request
    check r = r { Http.checkResponse = \rq rs ->
        when (responseStatus rs `notElem` [status200, status403]) $
            let ex = StatusCodeException (rs { responseBody = () }) mempty
            in throwM $ HttpExceptionRequest rq ex
    }

deleteUser :: UserId -> Galley ()
deleteUser uid = do
    (h, p) <- brigReq
    void $ call "brig"
        $ method DELETE . host h . port p
        . paths ["/i/users", toByteString' uid]
        . expect2xx

getContactList :: UserId -> Galley [UserId]
getContactList uid = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method GET . host h . port p
        . paths ["/i/users", toByteString' uid, "contacts"]
        . expect2xx
    cUsers <$> parseResponse (Error status502 "server-error") r
