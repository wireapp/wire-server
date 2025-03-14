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

-- | Time type conversion
module Wire.Sem.FromUTC where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Imports
import qualified SAML2.WebSSO.Types as SAML

-- | The class is helpful in interpreters for the 'Now' effect. It makes it
-- possible to use the interpreters for any time type so long as it implements
-- this single-method class.
class FromUTC a where
  fromUTCTime :: UTCTime -> a

instance FromUTC UTCTime where
  fromUTCTime = id

instance FromUTC SAML.Time where
  fromUTCTime = SAML.Time

instance FromUTC POSIXTime where
  fromUTCTime = utcTimeToPOSIXSeconds
