{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Types.Common where

import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import qualified Data.ByteString.Char8 as Bytes
import Data.ByteString.Conversion
import qualified Data.Text as Text
import Imports
import qualified Network.URI as Net

newtype CannonId
  = CannonId
      { cannonId :: Text
      }
  deriving
    ( Eq,
      Ord,
      Show,
      FromJSON,
      ToJSON,
      FromByteString,
      ToByteString
    )

newtype URI
  = URI
      { fromURI :: Net.URI
      }
  deriving (Eq, Ord, Show)

instance FromJSON URI where
  parseJSON = withText "URI" (parse . Text.unpack)

instance ToJSON URI where
  toJSON uri = String $ Text.pack (show (fromURI uri))

instance ToByteString URI where
  builder = builder . show . fromURI

instance FromByteString URI where
  parser = takeByteString >>= parse . Bytes.unpack

parse :: Monad m => String -> m URI
parse = maybe (fail "Invalid URI") (return . URI) . Net.parseURI
