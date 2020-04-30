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

module CargoHold.API
  ( sitemap,
  )
where

import qualified CargoHold.API.Public as Public
import CargoHold.App (Handler)
import Data.Predicate (true)
import qualified Data.Swagger.Build.Api as Doc
import Imports hiding (head)
import Network.Wai.Routing (Routes, continue, get, head)
import Network.Wai.Utilities (empty)

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = do
  Public.sitemap
  Public.apiDocs
  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true
