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
import Brig.API.Internal qualified as Internal
import Brig.API.Public qualified as Public
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Network.Wai.Routing (Routes)
import Polysemy

sitemap ::
  forall r p.
  ( Member BlacklistStore r,
    Member GalleyProvider r,
    Member (UserPendingActivationStore p) r
  ) =>
  Routes () (Handler r) ()
sitemap = do
  Public.sitemap
  Internal.sitemap
