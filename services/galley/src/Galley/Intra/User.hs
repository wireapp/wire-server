{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.User
    ( getConnections
    , deleteBot
    , getUserTeams
    , bindUser
    ) where

import Bilge hiding (options, getHeader)
import Brig.Types.Intra (UserTeam(..))
import Bilge.RPC
import Bilge.Retry
import Brig.Types.Intra (ConnectionStatus (..))
import Brig.Types.Connection (Relation (..))
import Galley.App
import Galley.Options
import Control.Monad (void)
import Control.Lens (view)
import Control.Retry
import Data.ByteString.Char8 (pack, intercalate)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Id
import Data.Misc (portNumber)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status502)
import Network.Wai.Utilities.Error

import qualified Data.Text.Lazy as LT

getConnections :: UserId -> [UserId] -> Maybe Relation -> Galley [ConnectionStatus]
getConnections u uids rlt = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    r <- call "brig"
        $ method GET . host h . port (portNumber p)
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
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    void $ call "brig"
        $ method DELETE . host h . port (portNumber p)
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

getUserTeams :: [UserId] -> Galley [UserTeam]
getUserTeams us = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    r <- call "brig"
        $ method GET . host h . port (portNumber p)
        . path "/i/users-teams"
        . queryItem "ids" users
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    users = intercalate "," $ toByteString' <$> us

bindUser :: TeamId -> UserId -> Galley ()
bindUser t u = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    void $ call "brig"
        $ method POST . host h . port (portNumber p)
        . paths ["/i/users", toByteString' u, "bind", toByteString' t]
        . expect2xx

-----------------------------------------------------------------------------
-- Helpers

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
