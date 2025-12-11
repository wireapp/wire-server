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

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as AK
import qualified Data.Aeson.Pointer as AD
import Data.Bifunctor (first)
import Imports
import Web.Scim.Schema.Common (lowerKey)
import Web.Scim.Schema.Error
import Web.Scim.Schema.Schema

-- package.  This type provides the parser for the scim patch syntax.
--
-- FUTUREWORK(fisx): some parts of the standard have been deemed exotic
-- enough to skip implementation (sometimes, there are pending tests
-- stubs for those things, sometimes not).  Example: value paths
-- (filter on arrays to select some attributes, this is handled with
-- indices in aeson-diff) are a scim thing and not supported here.
--
-- https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
-- https://datatracker.ietf.org/doc/html/rfc6901
newtype PatchOp = PatchOp AD.Patch
  deriving (Eq, Show)

-- | Allow only add, remove, replace, and disallow array indices (the
-- latter need to be translated into value search expressions in
-- scim).
--
-- Example:
--
--    { "schemas":
--        ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
--      "Operations":[
--        {
--         "op":"add",
--         "path":"members",
--         "value":[
--          {
--            "display": "Babs Jensen",
--            "$ref": "https://example.com/v2/Users/2819c223...413861904646",
--            "value": "2819c223-7f76-453a-919d-413861904646"
--          }
--         ]
--        },
--        ... + additional operations if needed ...
--      ]
--    }
validatePatchOp :: forall m. (MonadError String m) => PatchOp -> m ()
validatePatchOp (PatchOp (AD.Patch ops)) = opOk `mapM_` ops
  where
    opOk :: AD.Operation -> m ()
    opOk = \case
      AD.Add path _ -> pathOk path
      AD.Rem path -> pathOk path
      AD.Rep path _ -> pathOk path
      AD.Mov {} -> throwError "unsupported patch operation: mov"
      AD.Cpy {} -> throwError "unsupported patch operation: cpy"
      AD.Tst {} -> throwError "unsupported patch operation: tst"

    pathOk :: AD.Pointer -> m ()
    pathOk (AD.Pointer path) = keyOk `mapM_` path

    keyOk :: AD.Key -> m ()
    keyOk = \case
      AD.OKey {} -> pure ()
      AD.AKey {} -> throwError "unsupported key type: index" -- TODO: make this work!

instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \obj_ -> do
    let obj = AK.fromList . map (first lowerKey) . AK.toList $ obj_
    scms :: [Schema] <- obj .: "schemas"
    patchOp <- PatchOp <$> obj .: "operations"
    guard $ PatchOp20 `elem` scms
    either fail pure (validatePatchOp patchOp)
    pure patchOp

instance ToJSON PatchOp where
  toJSON (PatchOp operations) =
    object ["schemas" .= [PatchOp20], "operations" .= operations]

applyPatch :: forall m a. (FromJSON a, ToJSON a, MonadError ScimError m) => PatchOp -> a -> m a
applyPatch op a = do
  throwError (todo op (eitherDecode @a (encode a)) :: ScimError)
