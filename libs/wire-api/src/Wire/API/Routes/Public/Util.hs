{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Util where

import Data.SOP (I (..), NS (..))
import Servant
import Imports
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Routes.MultiVerb

instance
  (ResponseType r1 ~ a, ResponseType r2 ~ a) =>
  AsUnion '[r1, r2] (ResponseForExistedCreated a)
  where
  toUnion (Existed x) = Z (I x)
  toUnion (Created x) = S (Z (I x))

  fromUnion (Z (I x)) = Existed x
  fromUnion (S (Z (I x))) = Created x
  fromUnion (S (S x)) = case x of

-- Note: order is important here; if you swap Existed with Created, the wrong
-- status codes will be returned. Keep the Order in ResponseForExistedCreated
-- and the corresponding type the same.
data ResponseForExistedCreated a
  = Existed !a
  | Created !a
  deriving (Functor)

type ResponsesForExistedCreated eDesc cDesc a =
  '[ Respond 200 eDesc a,
     Respond 201 cDesc a
   ]

data UpdateResult a
  = Unchanged
  | Updated !a

type UpdateResponses unchangedDesc updatedDesc a =
  '[ RespondEmpty 204 unchangedDesc,
     Respond 200 updatedDesc a
   ]

instance
  (ResponseType r1 ~ (), ResponseType r2 ~ a) =>
  AsUnion '[r1, r2] (UpdateResult a)
  where
  toUnion Unchanged = inject (I ())
  toUnion (Updated a) = inject (I a)

  fromUnion (Z (I ())) = Unchanged
  fromUnion (S (Z (I a))) = Updated a
  fromUnion (S (S x)) = case x of
