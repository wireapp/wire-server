-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Internal.Brig.EnterpriseLogin where

import Servant
import Wire.API.Routes.Named
import Data.Domain

--------------------------------------------------------------------------------
-- API Internal

type EnterpriseLoginApi =
  Named
    "domain-registration-lock"
    ( Summary "Adds a domain to the Deny-list"
        :> Description "This creates an entry in the email domain registration table with domain-redirect=locked and team-invites=allowed. Any previous entry for that domain is overwritten."
        :> "domain-registration"
        :> Capture "domain" Domain
        :> "lock"
        :> Post '[JSON] NoContent
    )
