{-# LANGUAGE ViewPatterns #-}

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
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.KeyMap as AK
import qualified Data.Aeson.Pointer as AD
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Imports
import Web.Scim.Filter
import Web.Scim.Schema.Common (lowerKey)
import Web.Scim.Schema.Error
import Web.Scim.Schema.Schema

-- This type provides the parser for the scim patch syntax, and can be
-- turned into an `AD.Patch` with `validatePatchOp`.
--
-- Differences to AD.Patch:
-- - Only add, remove, replace.
-- - Point into array with filters, not indices.
-- - Case insensitive.
-- - The semantics is a bit convoluted and may diverge from that of
--   `AD.Patch` (see RFCs).
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
--
-- patch for scim: https://datatracker.ietf.org/doc/html/rfc7644#section-3.5.2
-- patch for json: https://datatracker.ietf.org/doc/html/rfc6901
newtype Patch = Patch {fromPatch :: [PatchOp]}
  deriving (Eq, Show)

data PatchOp
  = PatchOpAdd (Maybe AttrPath) Value
  | PatchOpRemove AttrPath
  | PatchOpReplace (Maybe AttrPath) Value
  deriving (Eq, Show)

----------------------------------------------------------------------

instance ToJSON Patch where
  toJSON = todo

instance ToJSON PatchOp where
  toJSON op =
    object $
      ["op" .= String (patchOpName op)]
        <> ["path" .= p | p <- maybeToList $ patchOpPath op]
        <> ["val" .= v | v <- maybeToList $ patchOpVal op]
    where
      patchOpName :: PatchOp -> Text
      patchOpName = \case
        PatchOpAdd _ _ -> "add"
        PatchOpRemove _ -> "remove"
        PatchOpReplace _ _ -> "replace"

      patchOpPath :: PatchOp -> Maybe AttrPath
      patchOpPath = \case
        PatchOpAdd mbp _ -> mbp
        PatchOpRemove p -> Just $ p
        PatchOpReplace mbp _ -> mbp

      patchOpVal :: PatchOp -> Maybe Value
      patchOpVal = \case
        PatchOpAdd _ v -> Just v
        PatchOpRemove _ -> Nothing
        PatchOpReplace _ v -> Just v

----------------------------------------------------------------------

instance FromJSON Patch where
  parseJSON = todo

instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \o -> do
    o .: "op" >>= \case
      "add" -> do
        path <- o .:? "path"
        val <- o .: "value"
        pure $ PatchOpAdd path val
      "remove" -> do
        path <- o .: "path"
        pure $ PatchOpRemove path
      "replace" -> do
        path <- o .:? "path"
        val <- o .: "value"
        pure $ PatchOpReplace path val
      unknownOp -> fail $ "Unknown operation: " ++ T.unpack unknownOp

{-

-- TODO: full SCIM path with filter expressions
scimPathToPointer :: Text -> AD.Pointer
scimPathToPointer = undefined -- path = map AD.OKey $ T.split (== '.') $ T.dropWhile (== '/') path

pointerToScimPath :: AD.Pointer -> Text
pointerToScimPath keys =
  {-
  T.intercalate "." $ map keyToText keys
  where
    keyToText (AD.OKey k) = k
    keyToText (AD.AKey i) = T.pack (show i)
  -}
  undefined

-}

----------------------------------------------------------------------

-- TODO: use this to apply a list of patches so we only have to call AD.patch once.
applyPatch :: forall m a. (FromJSON a, ToJSON a, MonadError ScimError m) => PatchOp -> a -> m a
applyPatch = todo

{-

applyPatch hscimOp (toJSON -> jsonOrig) = do
  patch <-
    validatePatchOp hscimOp
      & let err = throwError . badRequest InvalidSyntax . Just . Text.pack
         in either err pure
  jsonPatched <-
    AD.patch patch jsonOrig
      & let err = throwError . badRequest InvalidValue . Just . ("could not apply patch: " <>) . Text.pack
         in \case
              Success val -> pure val
              Error txt -> err txt
  fromJSON jsonPatched
    & let err = throwError . badRequest InvalidPath . Just . ("could not apply patch: " <>) . Text.pack
       in \case
            Success val -> pure val
            Error txt -> err txt

validatePatchOp :: forall m. (MonadError String m) => PatchOp -> Value -> m AD.Patch
validatePatchOp (PatchOp _) = do
  -- opOk `mapM_` undefined
  pure undefined
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

-}
