{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Cassandra.Settings
    ( Settings
    , defSettings
    , setProtocolVersion
    , setCompression
    , setContacts
    , addContact
    , setKeyspace
    , setPortNumber
    , setIdleTimeout
    , setMaxConnections
    , setMaxStreams
    , setPoolStripes
    , setConnectTimeout
    , setSendTimeout
    , setMaxTimeouts
    , setPrepareStrategy
    , setResponseTimeout
    , setRetrySettings
    , setPolicy
    , initialContacts
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Text (pack, stripSuffix, unpack)
import Database.CQL.IO
import Network.Wreq

initialContacts :: MonadIO m => String -> String -> m (NonEmpty String)
initialContacts (pack -> srv) url = liftIO $ do
    rs <- asValue =<< get url
    let srvs = case stripSuffix "_seed" srv of
                   Nothing -> [srv, srv <> "_seed"]
                   Just _  -> [srv] -- requesting only seeds is a valid use-case
    let ip = rs ^.. responseBody
                  . key "roles"
                  . members
                  . indices (`elem` srvs)
                  . values
                  . key "privateIpAddress"
                  . _String
                  & map unpack
    case ip of
        i:ii -> return (i :| ii)
        _    -> error "initial-contacts: no IP addresses found."
