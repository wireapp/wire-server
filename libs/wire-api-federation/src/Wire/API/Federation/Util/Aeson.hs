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

module Wire.API.Federation.Util.Aeson
  ( customEncodingOptions,
    CustomEncoded (..),
    CustomEncodedUnion (..),
    UnionMember (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Char as Char
import Data.SOP
import Data.SOP.NS
import GHC.Generics (Rep)
import Imports hiding (All)
import Network.HTTP.Types (statusCode)
import Servant.API (HasStatus (..), WithStatus (..), statusOf)
import Servant.API.Status (KnownStatus (..))

-- | Drops record field name prefixes (anything until the first upper-case char)
-- and turns the rest into snake_case.
--
-- For example, it converts @_recordFieldLabel@ into @field_label@.
customEncodingOptions :: Options
customEncodingOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (not . Char.isUpper)
    }

newtype CustomEncoded a = CustomEncoded {unCustomEncoded :: a}

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomEncoded a) where
  toJSON = genericToJSON @a customEncodingOptions . unCustomEncoded

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomEncoded a) where
  parseJSON = fmap CustomEncoded . genericParseJSON @a customEncodingOptions

newtype CustomEncodedUnion as = CustomEncodedUnion
  {unCustomEncodedUnion :: NS I as}

class HasStatus a => UnionMember a where
  memberToJSON :: a -> Value
  memberParseJSON :: Value -> Parser a

instance {-# OVERLAPPABLE #-} (FromJSON a, ToJSON a, HasStatus a) => UnionMember a where
  memberToJSON = toJSON
  memberParseJSON = parseJSON

instance (FromJSON a, ToJSON a, KnownStatus c) => UnionMember (WithStatus c a) where
  memberToJSON (WithStatus x) = toJSON x
  memberParseJSON = fmap WithStatus . parseJSON

getStatus :: forall a. HasStatus a => Int
getStatus = statusCode (statusOf (Proxy @a))

getStatuses :: forall as. All HasStatus as => [Int]
getStatuses = unK (cpara_SList (Proxy @HasStatus) (K []) g :: K [Int] as)
  where
    g :: forall y ys. (HasStatus y, All HasStatus ys) => K [Int] ys -> K [Int] (y ': ys)
    g = K . (getStatus @y :) . unK

instance All UnionMember as => ToJSON (CustomEncodedUnion as) where
  toJSON = cfoldMap_NS (Proxy @UnionMember) go . unCustomEncodedUnion
    where
      go :: forall a. UnionMember a => I a -> Value
      go (I x) =
        object
          [ "status" .= getStatus @a,
            "value" .= memberToJSON x
          ]

-- | A status code that is guaranteed to be one of those associated to the types in
-- the list.
data ValidStatus x as = ValidStatus Int x

impossibleStatus :: ValidStatus x '[] -> r
impossibleStatus _ = error "bug: unhandled status in ValidStatus"

validateStatus :: forall as x. All HasStatus as => Int -> x -> Maybe (ValidStatus x as)
validateStatus s v = guard (s `elem` getStatuses @as) $> ValidStatus s v

instance (SListI as, All UnionMember as, All HasStatus as) => FromJSON (CustomEncodedUnion as) where
  parseJSON = withObject "Union" $ \obj -> do
    (status :: Int) <- obj .: "status"
    (value :: Value) <- obj .: "value"
    vs <-
      maybe
        (fail ("unexpected status " <> show status))
        pure
        (validateStatus status value)
    fmap CustomEncodedUnion . either fail pure . sequence_NS $
      cana_NS (Proxy @UnionMember) impossibleStatus g vs
    where
      g :: forall y ys. UnionMember y => ValidStatus Value (y ': ys) -> Either (Either String y) (ValidStatus Value ys)
      g (ValidStatus s v)
        | s /= getStatus @y = Right (ValidStatus s v)
        | otherwise = Left (parseEither memberParseJSON v)
