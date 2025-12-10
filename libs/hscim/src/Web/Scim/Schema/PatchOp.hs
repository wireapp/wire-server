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

module Web.Scim.Schema.PatchOp where

import Control.Monad (guard)
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as AK
import Data.Aeson.Types
import Data.Bifunctor (first)
import Web.Scim.Schema.Common (lowerKey)
import Web.Scim.Schema.Schema (Schema (PatchOp20))

-- | For scim patches (both user and group), we use the aeson-diff
-- package.  This type provides the parser for the scim patch syntax.
--
-- FUTUREWORK: some parts of the standard have been deemed exotic
-- enough to skip implementation (sometimes, there are pending tests
-- stubs for those things, sometimes not).  Example: value paths
-- (filter on arrays to select some attributes, this is handled with
-- indices in aeson-diff) are a scim thing and not supported here.
-- one could implement them by storing an extra `Map Text ValueOption`
-- in `PatchOp` and using the `AD.Path`s in `AD.Patch` to reference
-- those, but then we could't just call AD.patch as is any more.
--
-- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
newtype PatchOp = PatchOp AD.Patch
  deriving (Eq, Show)

isLegalPatchOp :: PatchOp -> Bool
isLegalPatchOp (PatchOp (AD.Patch ops)) = all ok ops
  where
    ok :: AD.Operation -> Bool
    ok =
      -- only add, remove, replace.  maybe we can also explicitly
      -- scream about unsupposed value paths (array indices)?
      undefined

{-
 { "schemas":
       ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
     "Operations":[
       {
        "op":"add",
        "path":"members",
        "value":[
         {
           "display": "Babs Jensen",
           "$ref": "https://example.com/v2/Users/2819c223...413861904646",
           "value": "2819c223-7f76-453a-919d-413861904646"
         }
        ]
       },
       ... + additional operations if needed ...
     ]
   }

under "Operations" comes an AD.Patch.  we just need to forbid some operations.

-}

instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \obj_ -> do
    let obj = AK.fromList . map (first lowerKey) . AK.toList $ obj_
    scms :: [Schema] <- obj .: "schemas"
    patchOp <- PatchOp <$> obj .: "operations"
    guard $ PatchOp20 `elem` scms
    guard $ isLegalPatchOp patchOp
    pure patchOp

instance ToJSON PatchOp where
  toJSON (PatchOp operations) =
    object ["schemas" .= [PatchOp20], "operations" .= operations]

operationIsLegal :: AD.Operation -> Bool
operationIsLegal _op = undefined

applyPatch {- (FromJSON a, ToJSON a, MonadError ScimError m) => -} :: PatchOp -> a -> m a
applyPatch =
  -- errors out when:
  -- 1. parsing of patched value doesn't work
  -- 2. ?
  undefined
