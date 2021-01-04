{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CommaSeparatedList where

import Control.Lens ((?~))
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString.Conversion (FromByteString, List, fromList, parser, runParser)
import Data.Proxy (Proxy (..))
import Data.Range (Bounds, Range)
import Data.Swagger (CollectionFormat (CollectionCSV), SwaggerItems (SwaggerItemsPrimitive), SwaggerType (SwaggerString), ToParamSchema (..), items, type_)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Imports
import Servant (FromHttpApiData (..))

newtype CommaSeparatedList a = CommaSeparatedList {fromCommaSeparatedList :: [a]}
  deriving stock (Show, Eq)
  deriving newtype (Bounds)

instance FromByteString (List a) => FromHttpApiData (CommaSeparatedList a) where
  parseUrlPiece t =
    CommaSeparatedList . fromList <$> Bifunctor.first Text.pack (runParser parser $ encodeUtf8 t)

instance ToParamSchema (CommaSeparatedList a) where
  toParamSchema _ = mempty & type_ ?~ SwaggerString

instance (ToParamSchema a, ToParamSchema (Range n m [a])) => ToParamSchema (Range n m (CommaSeparatedList a)) where
  toParamSchema _ =
    toParamSchema (Proxy @(Range n m [a]))
      & items ?~ SwaggerItemsPrimitive (Just CollectionCSV) (toParamSchema (Proxy @a))
