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
import Data.Foldable (for_)
import Data.Id
import Data.Maybe (isNothing)
import Data.Predicate
import Gundeck.Env (awsEnv)
import Gundeck.Monad
import Gundeck.Push.Native.Types
import Gundeck.Util
import Network.HTTP.Types
import Network.Wai (Request, Response)
import Network.Wai.Utilities

import qualified Gundeck.Aws         as Aws
import qualified Gundeck.Client.Data as Clients
import qualified Gundeck.Push.Data   as Push

register :: UserId ::: ClientId ::: Request ::: JSON ::: JSON -> Gundeck Response
register (uid ::: cid ::: req ::: _) = do
    Clients.insert uid cid =<< fromBody req (Error status400 "bad-request")
    return empty

unregister :: UserId ::: ClientId -> Gundeck Response
unregister (uid ::: cid) = do
    keys <- Clients.select uid cid
    when (isNothing keys) $
        throwM (Error status404 "not-found" "Client not found")
    aa <- filter byClient <$> Push.lookup uid Push.Quorum
    for_ aa $ \a ->
        Push.delete (a^.addrUser) (a^.addrTransport) (a^.addrApp) (a^.addrToken)
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
    env <- view awsEnv
    let rm a = Aws.execute env (Aws.deleteEndpoint (a^.addrEndpoint))
    Push.lookup user Push.Quorum >>= mapM_ rm
    Push.erase user
    Clients.erase user
    return empty
