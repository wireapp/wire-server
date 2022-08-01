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

module Brig.API
  ( sitemap,
  )
where

import Brig.API.Handler (Handler)
import qualified Brig.API.Internal as Internal
import qualified Brig.API.Public as Public
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Sem.CodeStore
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import qualified Data.Swagger.Build.Api as Doc
import Network.Wai.Routing (Routes)
import Polysemy

sitemap ::
  Members
    '[ CodeStore,
       PasswordResetStore,
       BlacklistStore,
       BlacklistPhonePrefixStore
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
sitemap = do
  Public.sitemap
  Public.apiDocs
  Internal.sitemap
