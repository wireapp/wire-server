{-# LANGUAGE OverloadedStrings #-}

module Stern.API.Predicates
  ( phoneParam,
  )
where

import Brig.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Conversion
import qualified Data.Char as Char
import Imports
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Routing (param)
import Network.Wai.Routing.Request

phoneParam :: (HasCaptures r, HasQuery r) => Predicate r Error Phone
phoneParam = (>>= maybe (Fail invalidPhone) pure . fromByteString) <$> trimParam "phone"

trimParam :: (HasCaptures r, HasQuery r) => ByteString -> Predicate r Error ByteString
trimParam p = fmap (Char8.filter (not . Char.isSpace)) <$> param p

invalidPhone :: Error
invalidPhone = setMessage "Invalid phone number" e400
