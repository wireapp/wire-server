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

module Wire.SparAPIAccess.Rpc where

import Bilge hiding (head, options, requestId)
import Data.Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Imports
import Network.HTTP.Types.Method
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger.Message
import Util.Options
import Wire.API.User.IdentityProvider
import Wire.ParseException
import Wire.Rpc
import Wire.SparAPIAccess

interpretSparAPIAccessToRpc ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member TinyLog r
  ) =>
  Endpoint ->
  Sem (SparAPIAccess ': r) a ->
  Sem r a
interpretSparAPIAccessToRpc sparEndpoint =
  interpret $
    runInputConst sparEndpoint . \case
      GetIdentityProviders tid -> getIdentityProvidersImpl tid

sparRequest ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  (Request -> Request) ->
  Sem r (Response (Maybe LByteString))
sparRequest req = do
  ep <- input
  rpcWithRetries "spar" ep req

getIdentityProvidersImpl ::
  ( Member TinyLog r,
    Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  TeamId ->
  Sem r IdPList
getIdentityProvidersImpl tid = do
  debug $
    field "remote" ("spar" :: ByteString)
      . msg (val "get identity providers")
      . field "team" (toByteString tid)
  decodeBodyOrThrow "spar" =<< sparRequest getReq
  where
    getReq =
      method GET
        . paths ["i", "identity-providers", toByteString' tid]

-- FUTUREWORK: This is duplicated in Wire/GalleyAPIAccess/Rpc. Move to a common module.
decodeBodyOrThrow :: forall a r. (Typeable a, FromJSON a, Member (Error ParseException) r) => Text -> Response (Maybe BL.ByteString) -> Sem r a
decodeBodyOrThrow ctx r = either (throw . ParseException ctx) pure (responseJsonEither r)
