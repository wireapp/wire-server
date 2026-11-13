-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationAPIAccess.Galley
  ( sendMLSMessage,
    normalizeMLSMessageResponse,
  )
where

import Data.Json.Util
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.FederationAPIAccess
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | Select the endpoint using its version guards, then normalize the response.
-- Alternative only handles version mismatches; RPC failures are not retried.
sendMLSMessage ::
  ( Member (FederationAPIAccess FederatorClient) r,
    Member (Error FederationError) r,
    Member Now r
  ) =>
  Remote x ->
  MLSMessageSendRequest ->
  Sem r (Either MLSMessageResponse MLSMessageSendSuccess)
sendMLSMessage remote request = do
  response <-
    runFederated remote $
      (Right <$> fedClient @'Galley @"send-mls-message" request)
        <|> (Left <$> fedClient @'Galley @"send-mls-message@v4" request)
  normalizeMLSMessageResponse response

normalizeMLSMessageResponse ::
  (Member Now r) =>
  Either MLSMessageResponse MLSMessageResponseV5 ->
  Sem r (Either MLSMessageResponse MLSMessageSendSuccess)
normalizeMLSMessageResponse = \case
  Left (MLSMessageResponseUpdates updates) -> do
    -- Legacy responses have no event timestamp, so retain their clock fallback.
    timestamp <- toUTCTimeMillis <$> Now.get
    pure $ Right (MLSMessageSendSuccess updates timestamp)
  Left failure -> pure $ Left failure
  Right (MLSMessageResponseV5Success success) -> pure $ Right success
  Right (MLSMessageResponseV5Error (MLSMessageResponseUpdates _)) ->
    pure $ Left (MLSMessageResponseProtocolError "unexpected success in federation error response")
  Right (MLSMessageResponseV5Error failure) -> pure $ Left failure
