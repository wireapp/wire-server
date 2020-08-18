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

module Federator.Remote
  ( joinConversationById,
  )
where

import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Id (ConvId)
import Data.Qualified (Qualified)
import Data.String.Conversions (cs)
import Federator.App (AppIO)
import Federator.Error (internalErrorWithDescription)
import Federator.Types (manager)
import Imports
import Servant.Client (ClientError (FailureResponse), ClientM, mkClientEnv, runClientM)
import Servant.Client.Core (BaseUrl)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import qualified Wire.API.Federation.Conversation as Fed
import qualified Wire.API.Federation.Types.Event as Fed

joinConversationById :: BaseUrl -> Qualified ConvId -> Fed.JoinConversationByIdRequest -> AppIO (Fed.ConversationUpdateResult Fed.MemberJoin)
joinConversationById = Fed.joinConversationById . client

client :: BaseUrl -> Fed.Api (AsClientT AppIO)
client baseUrl = genericClientHoist nat
  where
    nat :: ClientM a -> AppIO a
    nat clientM = do
      env <-
        mkClientEnv
          <$> view manager
          <*> pure baseUrl
      -- We run the same error handler for all endpoints.
      -- You can handle errors in each function by instead making
      -- `client :: API (AsClientT (ExceptT ClientError Galley))`
      handleClientError =<< liftIO (runClientM clientM env)

-- TODO(federation): we need more robust error handling
handleClientError :: Either ClientError a -> AppIO a
handleClientError = \case
  Right res ->
    pure res
  Left (FailureResponse _req rsp) ->
    throwM . internalErrorWithDescription . cs $
      "received error response from remote backend:\n" <> show rsp
  Left otherError ->
    throwM . internalErrorWithDescription . cs $
      "error when calling remote backend:\n" <> show otherError
