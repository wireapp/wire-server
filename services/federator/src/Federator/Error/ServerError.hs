-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.Error.ServerError where

import Data.Text.Lazy qualified as LText
import Federator.Error
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Federation.Domain
import Wire.Sem.Logger.Level qualified as Log

data ServerError
  = InvalidRoute
  | UnknownComponent Text
  | NoOriginDomain
  deriving (Eq, Show, Typeable)

instance Exception ServerError

instance AsWai ServerError where
  errorLogLevel _ = Log.Error
  toWai e@InvalidRoute =
    Wai.mkError HTTP.status403 "invalid-endpoint" (LText.fromStrict (serverErrorDescription e))
  toWai e@(UnknownComponent _) =
    Wai.mkError HTTP.status403 "unknown-component" (LText.fromStrict (serverErrorDescription e))
  toWai e@NoOriginDomain =
    Wai.mkError HTTP.status403 "no-origin-domain" (LText.fromStrict (serverErrorDescription e))

serverErrorDescription :: ServerError -> Text
serverErrorDescription InvalidRoute = "The requested endpoint does not exist"
serverErrorDescription (UnknownComponent name) = "No such component: " <> name
serverErrorDescription NoOriginDomain = "No " <> originDomainHeaderName <> " header"
