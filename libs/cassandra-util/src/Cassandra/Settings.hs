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
    , initialContactsDisco
    , initialContactsDNS
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Database.CQL.IO hiding (values)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack, stripSuffix, unpack, Text)
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.Wreq

import qualified Data.List.NonEmpty as NE

initialContactsDisco :: MonadIO m => String -> String -> m (NonEmpty String)
initialContactsDisco (pack -> srv) url = liftIO $ do
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

initialContactsDNS :: MonadIO m => Text -> m (NonEmpty String)
initialContactsDNS address = liftIO $ do
    rs  <- makeResolvSeed defaultResolvConf
    ips <- withResolver rs $ \resolver -> lookupA resolver (encodeUtf8 address)
    return $ case ips of
      Right (x:xs) -> NE.map show (x :| xs)
      _            -> fallback
  where
    fallback = unpack address :| [] -- If it's not a valid DNS name, just try using it anyway
