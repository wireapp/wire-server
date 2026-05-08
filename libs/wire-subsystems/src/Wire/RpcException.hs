{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

-- | See also: "Wire.ParseException"
module Wire.RpcException where

import Bilge
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as LText
import Imports
import Network.HTTP.Types.Status qualified as Http
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error

data RpcException
  = RpcExceptionWai
      { service :: Text,
        waiError :: Wai.Error
      }
  | RpcExceptionInternal
      { service :: Text,
        status :: Int,
        message :: Text
      }
  deriving (Eq, Show)

instance Exception RpcException

rpcExcepctionToWaiError :: RpcException -> Wai.Error
rpcExcepctionToWaiError (RpcExceptionWai {..}) =
  waiError {Wai.message = "[" <> LText.fromStrict service <> "] " <> (Wai.message waiError)}
rpcExcepctionToWaiError (RpcExceptionInternal {..}) =
  Wai.mkError
    Http.status502
    "internal-error"
    ( LText.fromStrict $
        "Could not parse "
          <> service
          <> " response body: "
          <> message
          <> " (status: "
          <> Text.pack (show status)
          <> ")"
    )

-- | If a call to another backend service fails, just respond with whatever it said.
rethrow :: (HasCallStack, Member (Error RpcException) r) => Text -> ResponseLBS -> Sem r a
rethrow serviceName resp = throw err
  where
    err :: RpcException
    err = maybe fallback (RpcExceptionWai serviceName) (responseJsonMaybe resp)

    fallback :: RpcException
    fallback = RpcExceptionInternal serviceName (Bilge.statusCode resp) (maybe "" (decodeUtf8 . LBS.toStrict) (responseBody resp))
