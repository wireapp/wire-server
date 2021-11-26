-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

import qualified Data.Text.Lazy as LText
import Federator.Error
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Wire.API.Federation.Domain

data ServerError
  = InvalidRoute
  | UnknownComponent Text
  | NoOriginDomain
  deriving (Eq, Show, Typeable)

instance Exception ServerError

instance AsWai ServerError where
  toWai e@InvalidRoute =
    Wai.mkError HTTP.status404 "no-endpoint" (LText.fromStrict (waiErrorDescription e))
  toWai e@(UnknownComponent _) =
    Wai.mkError HTTP.status404 "unknown-component" (LText.fromStrict (waiErrorDescription e))
  toWai e@NoOriginDomain =
    Wai.mkError HTTP.status403 "no-origin-domain" (LText.fromStrict (waiErrorDescription e))

  waiErrorDescription InvalidRoute = "The requested endpoint does not exist"
  waiErrorDescription (UnknownComponent name) = "No such component: " <> name
  waiErrorDescription NoOriginDomain = "No " <> originDomainHeaderName <> " header"
