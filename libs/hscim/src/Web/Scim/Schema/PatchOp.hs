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

module Web.Scim.Schema.PatchOp
  ( PatchOp (..),
    isLegalPatchOp,
    operationIsLegal,
    applyPatch,
  )
where

import Control.Monad (guard)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as AK
import Data.Aeson.Types
import Data.Bifunctor (first)
import Data.Text (pack)
import Web.Scim.Schema.Common (lowerKey)
import Web.Scim.Schema.Error (ScimError, ScimErrorType (..), badRequest)
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

-- | Validates that a PatchOp contains only legal operations.
-- Currently supports Add, Rem (Remove), and Rep (Replace) operations.
-- Rejects operations that reference array indices or other unsupported path components.
--
-- FUTUREWORK: Currently rejects all multi-valued attribute operations and
-- operations with array index references. This is a conservative implementation
-- that prioritizes correctness over full SCIM spec support.
isLegalPatchOp :: PatchOp -> Bool
isLegalPatchOp (PatchOp (AD.Patch ops)) = all operationIsLegal ops

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

-- | Checks if an individual AD.Operation is legal according to SCIM constraints.
-- Only Add, Rem (Remove), and Rep (Replace) operations are supported.
-- Operations with array indices or complex path operations (Mov, Cpy, Tst) are rejected.
--
-- FUTUREWORK: This is intentionally conservative. Multi-valued attributes and
-- value-path filters are not supported.
operationIsLegal :: AD.Operation -> Bool
operationIsLegal op = case op of
  -- Allow Add, Rem, Rep with paths that don't contain array indices
  AD.Add path _ -> pathIsLegal path
  AD.Rem path -> pathIsLegal path
  AD.Rep path _ -> pathIsLegal path
  -- Reject Mov, Cpy, Tst operations (not part of basic SCIM PATCH)
  AD.Mov {} -> False
  AD.Cpy {} -> False
  AD.Tst {} -> False

-- | Checks if a path contains only keys (not array indices).
-- SCIM paths like "userName", "name.givenName" are allowed.
-- Paths with array indices like "emails[0]" are rejected.
pathIsLegal :: AD.Path -> Bool
pathIsLegal path = all isKeyItem path
  where
    isKeyItem :: AD.Key -> Bool
    isKeyItem (AD.OKey _) = True
    isKeyItem (AD.OIndex _) = False

-- | Applies a PatchOp to a value of type 'a' (which must be ToJSON/FromJSON).
-- This function:
-- 1. Validates all operations are legal (no unsupported ops or array indices)
-- 2. Converts the value to JSON
-- 3. Applies the aeson-diff patch
-- 4. Parses the result back to type 'a'
-- 5. Validates that immutable/read-only attributes haven't been changed illegally
--
-- Errors are mapped to appropriate ScimError types:
-- - InvalidPath: for unsupported operation types or paths
-- - InvalidValue: for parse failures or invalid resulting values
-- - Mutability: for attempts to modify read-only fields (FUTUREWORK)
--
-- FUTUREWORK: Currently we don't check immutability constraints (like preventing
-- changes to 'id' or 'meta' fields). This should be added in a future iteration.
applyPatch :: (FromJSON a, ToJSON a, MonadError ScimError m) => PatchOp -> a -> m a
applyPatch patchOp@(PatchOp adPatch) value = do
  -- Step 1: Validate that all operations in the patch are legal
  if not (isLegalPatchOp patchOp)
    then
      throwError $
        badRequest
          InvalidPath
          (Just "Unsupported patch operation: multi-valued attributes and array operations are not supported")
    else do
      -- Step 2: Convert the value to JSON
      let jsonValue = toJSON value

      -- Step 3: Apply the aeson-diff patch
      -- AD.patch returns Either String Value
      case AD.patch adPatch jsonValue of
        Left err ->
          -- Patch application failed (e.g., path doesn't exist, type mismatch)
          throwError $
            badRequest
              InvalidValue
              (Just $ pack err)
        Right patchedValue -> do
          -- Step 4: Parse the patched JSON back to type 'a'
          case fromJSON patchedValue of
            Error reason ->
              -- Parsing failed - the patched value doesn't conform to type 'a'
              throwError $
                badRequest
                  InvalidValue
                  (Just $ pack reason)
            Success result -> do
              -- Step 5: FUTUREWORK - validate immutability constraints
              -- For now, we skip this check. In the future, we should:
              -- - Extract 'id' from original and result (if they have id field)
              -- - Compare and reject if they differ
              -- - Similar checks for 'meta' and other read-only fields
              -- if originalId /= resultId then
              --   throwError $ badRequest Mutability (Just "Cannot modify id field")
              -- else
              pure result
