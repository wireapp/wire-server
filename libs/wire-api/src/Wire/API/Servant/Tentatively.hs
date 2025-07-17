-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Servant.Tentatively
  ( Tentatively (Tentatively),
    AccessGranted,
    forceTentatively,
  )
where

import Data.Bifunctor
import Data.ByteString.Lazy.Char8 as LBS
import Data.Text qualified as T
import Imports
import Servant
import Servant.API.Extended

-- | Parse data coming in through servant only *tentatively* until after the handler has
-- carried out some dynamic access control.
--
-- FUTUREWORK: can we hide authentication and access control deeper in servant such that the
-- handler doesn't need to explicitly call it?
newtype Tentatively a = Tentatively {forceTentatively :: AccessGranted -> Either Text a}

-- | Token to give to a tentatively parsed value to force the parser function.
--
-- FUTUREWORK: we could make this abstract and only allow the access control code to construct
-- any values, then the application logic would not be able to parse unauthorized data even by
-- accident.  But that would require some rethinking how and where we do access control.
type AccessGranted = ()

instance {-# OVERLAPPING #-} (FromHttpApiData a) => FromHttpApiData (Tentatively a) where
  parseUrlPiece raw =
    pure . Tentatively $ \() -> parseUrlPiece @a raw

instance {-# OVERLAPPING #-} (MimeUnrender ct a) => MimeUnrender ct (Tentatively a) where
  mimeUnrender proxy payload =
    pure . Tentatively $ \() -> first T.pack $ mimeUnrender proxy payload

instance MakeCustomError "internal-error" (Tentatively a) where
  makeCustomError msg =
    err500
      { errBody =
          "parsing part of a request wrapped in `Tentatively` failed with "
            <> LBS.pack (show msg)
            <> ".  this is a bug in wire-server."
      }

-- TODO: move this module to extended?
