{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.Routes.Public.Util where

import Control.Comonad
import Data.Maybe
import Data.SOP (I (..), NS (..))
import Data.Type.Equality
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Routes.MultiVerb

instance
  (ResponseType r1 ~ a, ResponseType r2 ~ a) =>
  AsUnion '[r1, r2] (ResponseForExistedCreated a)
  where
  toUnion (Existed x) = Z (I x)
  toUnion (Created x) = S (Z (I x))

  fromUnion (Z (I x)) = Existed x
  fromUnion (S (Z (I x))) = Created x
  fromUnion (S (S x)) = case x of {}

-- Note: order is important here; if you swap Existed with Created, the wrong
-- status codes will be returned. Keep the Order in ResponseForExistedCreated
-- and the corresponding type the same.
data ResponseForExistedCreated a
  = Existed !a
  | Created !a
  deriving (Functor)

instance Comonad ResponseForExistedCreated where
  extract (Existed x) = x
  extract (Created x) = x

  duplicate r@(Existed _) = Existed r
  duplicate r@(Created _) = Created r

type ResponsesForExistedCreated eDesc cDesc a =
  '[ Respond 200 eDesc a,
     Respond 201 cDesc a
   ]

data UpdateResult a
  = Unchanged
  | Updated !a
  deriving (Functor)

mkUpdateResult :: Maybe a -> UpdateResult a
mkUpdateResult = maybe Unchanged Updated

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
  fromUnion (S (S x)) = case x of {}

type PaginationDocs =
  "The IDs returned by this endpoint are paginated. To get the first page, make\
  \ a call with the `paging_state` field set to `null` (or omitted). Whenever the\
  \ `has_more` field of the response is set to `true`, more results are available,\
  \ and they can be obtained by calling the endpoint again, but this time passing\
  \ the value of `paging_state` returned by the previous call. One can continue in\
  \ this fashion until all results are returned, which is indicated by `has_more`\
  \ being `false`. Note that `paging_state` should be considered an opaque token.\
  \ It should not be inspected, or stored, or reused across multiple unrelated\
  \ invocations of the endpoint."
