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

module Galley.API.License where

import Data.Id (UserId)
import Data.Qualified (Local)
import Imports
import Polysemy
import Wire.API.License qualified as API
import Wire.Licensing.Effect qualified as Licensing
import Wire.Licensing.License qualified as Licensing

getLicense :: (Member Licensing.Licensing r) => Local UserId -> Sem r API.License
getLicense _user =
  pure . toAPILicense =<< Licensing.getLicense
  where
    toAPILicense
      ( Licensing.License
          { meetings = Licensing.Meetings {Licensing.invitationLinks, Licensing.emailsSending, Licensing.calendarsIntegration}
          }
      ) =
        API.License
          { meetings =
              API.Meetings
                { invitationLinks,
                  emailsSending,
                  calendarsIntegration
                }
          }
