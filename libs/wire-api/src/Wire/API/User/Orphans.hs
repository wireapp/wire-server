{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.User.Orphans where

import Control.Lens ((.~))
import Data.CaseInsensitive (CI)
import Data.Data (Proxy (Proxy))
import Data.ISO3166_CountryCodes hiding (CI)
import Data.LanguageCodes
import Data.Swagger (ToSchema (..), description)
import Data.Swagger.Internal.Schema (paramSchemaToSchema, unnamed)
import Imports

deriving instance Generic ISO639_1

instance ToSchema ISO639_1

instance ToSchema CountryCode

instance ToSchema (CI Text) where
  declareNamedSchema _ =
    pure $
      unnamed $
        paramSchemaToSchema (Proxy @Text)
          & description .~ (Just "case insensitive string")
