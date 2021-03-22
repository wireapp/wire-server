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

module Wire.API.Federation.GRPC.Client where

import Control.Exception
import qualified Data.Text as T
import Imports
import Mu.GRpc.Client.Record (setupGrpcClient')
import Network.GRPC.Client.Helpers

newtype GrpcClientErr = GrpcClientErr {reason :: Text}
  deriving (Show, Eq)

-- | Note: setupGrpcClient' is unsafe and throws exceptions in IO, e.g. when it can't connect.
-- FUTUREWORK(federation): report setupGrpcClient' buggy behaviour to upstream.
createGrpcClient :: MonadIO m => GrpcClientConfig -> m (Either GrpcClientErr GrpcClient)
createGrpcClient cfg = do
  res <- liftIO $ try @IOException $ setupGrpcClient' cfg
  pure $ case res of
    Left err -> Left (GrpcClientErr (T.pack (show err <> errorInfo)))
    Right (Left err) -> Left (GrpcClientErr (T.pack (show err <> errorInfo)))
    Right (Right client) -> Right client
  where
    errorInfo =
      "Host: " <> show (_grpcClientConfigHost cfg)
        <> (" Port: " <> show (_grpcClientConfigPort cfg))
