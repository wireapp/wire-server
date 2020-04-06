{-# LANGUAGE OverloadedStrings #-}

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
