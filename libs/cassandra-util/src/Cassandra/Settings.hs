{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | This module exports types and functions from Database.CQL.IO, while adding a few wire specific functions.
module Cassandra.Settings
  ( module C,
    initialContactsDisco,
    initialContactsPlain,
  )
where

import Control.Lens
import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (pack, stripSuffix, unpack)
import Database.CQL.IO as C (Policy, Settings, addContact, defSettings, setCompression, setConnectTimeout, setContacts, setIdleTimeout, setKeyspace, setLogger, setMaxConnections, setMaxStreams, setMaxTimeouts, setPolicy, setPoolStripes, setPortNumber, setPrepareStrategy, setProtocolVersion, setResponseTimeout, setRetrySettings, setSendTimeout)
import Database.CQL.IO.Tinylog as C (mkLogger)
import Imports
import Network.Wreq

-- | This function is likely only useful at Wire, as it is Wire-infra specific.
-- Given a server name and a url returning a wire-custom "disco" json (AWS describe-instances-like json), e.g.
-- { "roles" : { "server_name": [ {"privateIpAddress": "...", ...}, {...} ] } },
-- return a list of IP addresses.
initialContactsDisco :: MonadIO m => String -> String -> m (NonEmpty String)
initialContactsDisco (pack -> srv) url = liftIO $ do
  rs <- asValue =<< get url
  let srvs = case stripSuffix "_seed" srv of
        Nothing -> [srv, srv <> "_seed"]
        Just _ -> [srv] -- requesting only seeds is a valid use-case
  let ip =
        rs
          ^.. responseBody
            . key "roles"
            . members
            . indices (`elem` srvs)
            . values
            . key "privateIpAddress"
            . _String
          & map unpack
  case ip of
    i : ii -> return (i :| ii)
    _ -> error "initial-contacts: no IP addresses found."

-- | Puts the address into a list using the same signature as the other initialContacts
initialContactsPlain :: MonadIO m => Text -> m (NonEmpty String)
initialContactsPlain address = pure $ unpack address :| []
