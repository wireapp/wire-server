module Wire.API.Federation.Util.Aeson
  ( customEncodingOptions,
    CustomEncoded (..),
  )
where

import Data.Aeson
import qualified Data.Char as Char
import GHC.Generics (Rep)
import Imports

customEncodingOptions :: Options
customEncodingOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (not . Char.isUpper)
    }

newtype CustomEncoded a
  = CustomEncoded {unCustomEncoded :: a}

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomEncoded a) where
  toJSON = genericToJSON @a customEncodingOptions . unCustomEncoded

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomEncoded a) where
  parseJSON = fmap CustomEncoded . genericParseJSON @a customEncodingOptions
