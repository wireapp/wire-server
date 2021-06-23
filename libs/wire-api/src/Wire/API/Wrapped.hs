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

module Wire.API.Wrapped where

import Control.Lens ((.~), (?~))
import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Proxy (Proxy (..))
import Data.Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Imports
import Test.QuickCheck (Arbitrary (..))

-- | Used for wrapping request or response types so we always accept and return
-- JSON maps
newtype Wrapped (name :: Symbol) a = Wrapped {unwrap :: a}
  deriving stock (Show, Eq)

instance (ToJSON a, KnownSymbol name) => ToJSON (Wrapped name a) where
  toJSON (Wrapped thing) = object [Text.pack (symbolVal (Proxy @name)) .= thing]

instance (FromJSON a, KnownSymbol name) => FromJSON (Wrapped name a) where
  parseJSON = withObject ("Wrapped" <> symbolVal (Proxy @name)) $ \o ->
    Wrapped <$> o .: Text.pack (symbolVal (Proxy @name))

-- | Creates schema without name, as coming up with a _nice_ name is fairly hard
-- here.
instance (ToSchema a, KnownSymbol name) => ToSchema (Wrapped name a) where
  declareNamedSchema _ = do
    let wrappedSchema = Inline (toSchema (Proxy @a))
    pure $
      NamedSchema Nothing $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ InsOrdHashMap.singleton (Text.pack (symbolVal (Proxy @name))) wrappedSchema

instance (Arbitrary a, KnownSymbol name) => Arbitrary (Wrapped name a) where
  arbitrary = Wrapped <$> arbitrary
