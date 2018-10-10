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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Text (pack, stripSuffix, unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Database.CQL.IO
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.Wreq

import qualified Data.List.NonEmpty as NE

-- | This function is likely only useful at Wire, as it is AWS/describe-instances specific.
-- Given a server name and a url returning a wire-custom "disco" json (AWS describe-instances-like json), e.g.
-- { "roles" : { "server_name": [ {...}, {...} ] } },
-- return a list of IP addresses.
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

-- | Given a DNS name or single IP, return a list of IP addresses.
initialContactsDNS :: MonadIO m => Text -> m (NonEmpty String)
initialContactsDNS address = liftIO $ do
    rs  <- makeResolvSeed defaultResolvConf
    ips <- withResolver rs $ \resolver -> lookupA resolver (encodeUtf8 address)
    return $ case ips of
      Right (x:xs) -> NE.map show (x :| xs)
      _            -> fallback
  where
    fallback = unpack address :| [] -- If it's not a valid DNS name, just try using it anyway
