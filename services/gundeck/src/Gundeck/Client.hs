{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Gundeck.Client
    ( register
    , unregister
    , lookupKeys
    , removeUser
    ) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Lens (view, (^.), set)
import Control.Monad
import Control.Monad.Catch
import Data.Id
import Data.Maybe (isNothing)
import Data.Predicate
import Gundeck.Monad
import Gundeck.Push.Native
import Gundeck.Util
import Network.HTTP.Types
import Network.Wai (Request, Response)
import Network.Wai.Utilities

import qualified Gundeck.Client.Data       as Clients
import qualified Gundeck.Notification.Data as Notifications
import qualified Gundeck.Push.Data         as Push

register :: UserId ::: ClientId ::: Request ::: JSON ::: JSON -> Gundeck Response
register (uid ::: cid ::: req ::: _) = do
    Clients.insert uid cid =<< fromBody req (Error status400 "bad-request")
    return empty

unregister :: UserId ::: ClientId -> Gundeck Response
unregister (uid ::: cid) = do
    keys <- Clients.select uid cid
    when (isNothing keys) $
        throwM (Error status404 "not-found" "Client not found")
    toks <- filter byClient <$> Push.lookup uid Push.Quorum
    deleteTokens toks Nothing
    Clients.remove uid cid
    return empty
  where
    byClient = (cid ==) . view addrClient

lookupKeys :: [Address "no-keys"] -> Gundeck [Address "keys"]
lookupKeys = mapConcurrently $ \a -> do
    k <- Clients.select (a^.addrUser) (a^.addrClient)
    return (set addrKeys k a)

removeUser :: UserId -> Gundeck Response
removeUser user = do
    toks <- Push.lookup user Push.Quorum
    deleteTokens toks Nothing
    Push.erase user
    Clients.erase user
    Notifications.deleteAll user
    return empty
