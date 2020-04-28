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

module Gundeck.API
  ( sitemap,
  )
where

import Data.Swagger.Build.Api hiding (Response, def, min)
import qualified Gundeck.API.Internal as Internal
import qualified Gundeck.API.Public as Public
import Gundeck.Monad
import Imports hiding (head)
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities

sitemap :: Routes ApiBuilder Gundeck ()
sitemap = do
  Public.sitemap
  Public.apiDocs
  Internal.sitemap
  head "/i/status" (continue $ const (return empty)) true
  get "/i/status" (continue $ const (return empty)) true
