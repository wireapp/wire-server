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

module Wire.API.ConverProtoLens where

import Data.Bifunctor (Bifunctor (first))
import Imports

-- | This typeclass exists to provide overloaded function names for convertion
-- between data types generated by proto-lens and data types used in wire
-- We added fundeps here for better type inference, but we can't be as explicit as we wanted
-- with @a -> b, b -> a@, since our instances would be orphaned on the left hand side argument.
class ConvertProtoLens a b | b -> a where
  fromProtolens :: a -> Either Text b
  toProtolens :: b -> a

-- | Add labels to error messages
protoLabel :: Text -> Either Text a -> Either Text a
protoLabel lbl = first ((lbl <> ": ") <>)
