{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Brig.Federation.Client where

import Brig.API.Error (notFound, throwStd)
import Brig.API.Handler (Handler)
import Brig.App (federator)
import Brig.Types.User
import Control.Lens (view, (^.))
import Data.Handle
import Data.Qualified
import Data.String.Conversions
import qualified Data.Text as T
import Imports
import Network.Socket (HostName, PortNumber)
import qualified System.Logger.Class as Log
import Util.Options (epHost, epPort)

-- TODO
-- import qualified Wire.API.Federation.GRPC.Proto as Proto
-- import Wire.API.Federation.GRPC.Service

-- WARNING: this function is experimental to check basic networking for federation and must not make its way to develop in its current form!
-- Problems:
-- this can go away once we make use of the types from router.proto
-- Also, validation is missing, grpc clients are not re-used,
-- types are coerced into other types without checking them.
--
-- getUserHandleInfo :: (MonadLogger m, MonadIO m, MonadError ServerError m) => Qualified Handle -> m (Maybe UserHandleInfo)
getUserHandleInfo :: Qualified Handle -> Handler (Maybe UserHandleInfo)
getUserHandleInfo _handle = do
  Log.warn $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  pure Nothing -- TODO
  -- (federatorHost, federatorPort) <- federationCheck
  -- let handle' = Proto.QualifiedHandle (domainText (qDomain handle)) (fromHandle (qUnqualified handle))

-- Log.warn $ Log.msg $ T.pack ("Brig-federation: handle: " <> show handle')
-- -- we should probably find a way to integrate MonadError (mu-)ServerError into a brig monad
-- response <- runExceptT $ iGetUserIdByHandle' federatorHost federatorPort handle'
-- case response of
--   Left err -> do
--     Log.warn $ Log.msg ("error on getUserHandleInfo in Brig/Federation/Client" <> show err)
--     throwStd (notFound (cs $ show err)) -- TODO better error handling
--   Right r -> do
--     let rDom = Domain (Proto.idDomain r) -- unsafe conversion from text here
--     let mrId = fromByteString' $ cs $ Proto.id r
--     case mrId of
--       Nothing -> do
--         Log.warn $ Log.msg $ T.pack ("error on converting to Qualified Id: " <> show r)
--         pure Nothing -- this should throw an error, not give a 404, as it's a type conversion problem
--       Just rId -> do
--         Log.warn $ Log.msg $ T.pack ("Brig-federation: result: " <> show rId)
--         pure $ Just (UserHandleInfo (Qualified rId rDom))

-- if the federator is not specified in brig's config, federation is assumed to be disabled
-- TODO document or change this behaviour!
federationCheck :: Handler (HostName, PortNumber)
federationCheck = do
  x <- view federator
  case x of
    Nothing -> throwStd (notFound "federation disabled") -- TODO better error message?
    Just y -> do
      let federatorHost = cs $ y ^. epHost
      let federatorPort = fromIntegral $ y ^. epPort
      return (federatorHost, federatorPort)
