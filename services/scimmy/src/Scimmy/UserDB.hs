{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Scimmy.UserDB where

import Control.Monad.IO.Class (liftIO)
import Scimmy.RPC
import Data.Id
import Data.Text.Encoding
import qualified DB.User as SCIM
import qualified Schema.User as SCIM

import qualified Bilge.RPC  as RPC
import qualified Bilge  as RPC

data Env a = Env
    { brig :: RPC.Request
    , galley :: RPC.Request
    , tid :: TeamId
    } deriving (Show)


instance SCIM.UserDB AppIO where
    list = undefined

    get :: SCIM.UserId -> AppIO (Maybe SCIM.StoredUser)
    get = getUser



getUser :: SCIM.UserId -> AppIO (Maybe SCIM.StoredUser)
getUser uid = do
    liftIO $ putStrLn "Hello"


    _res <- RPC.rpc "brig" (RPC.paths ["/i/users", encodeUtf8 uid])
    return Nothing
