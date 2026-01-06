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
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AK
import qualified Data.Aeson.Patch as AD
import qualified Data.Aeson.Pointer as AD
import Data.Aeson.Types
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Vector as V
import Imports
import Web.Scim.AttrName
import Web.Scim.Filter
import Web.Scim.Schema.Common
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
-- The Schemas associated with `tag` are only validated in
-- `applyPatch`.  We could do validation in `jsonPatchToScimPatch`,
-- but that seemed unnecessarily complicated.
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
newtype Patch tag = Patch {fromPatch :: [PatchOp tag]} -- TODO: rename to `ScimPatch`, and PatchOp to `ScimPatchOp`?
  deriving (Eq, Show)

data PatchOp tag
  = PatchOpAdd (Maybe ValuePath) Value
  | PatchOpRemove ValuePath
  | PatchOpReplace (Maybe ValuePath) Value
  deriving (Eq, Show)

----------------------------------------------------------------------

-- | Compute a patch operation for the aeson-diff package.  The
-- `Value` argument is needed to compute absolute indices into arrays
-- from the filter expressions in the scim patch.
--
-- Scim schema information in `AttrName`s is ignored (`AD.Patch` does
-- not do schema validation).
scimPatchToJsonPatch :: forall tag. Patch tag -> Value -> AD.Patch
scimPatchToJsonPatch (Patch scimOps) jsonOrig = do
  AD.Patch (concat (mapOp <$> scimOps))
  where
    mapOp :: PatchOp tag -> [AD.Operation]
    mapOp = \case
      PatchOpAdd mbAttrPath val -> (`AD.Add` val) <$> mapPath mbAttrPath
      PatchOpRemove attrPath -> AD.Rem <$> mapPath (Just attrPath)
      PatchOpReplace mbAttrPath val -> (`AD.Rep` val) <$> mapPath mbAttrPath

    mapPath :: Maybe ValuePath -> [AD.Pointer]
    mapPath Nothing = [emptyPath]
    mapPath (Just (ValuePath (AttrPath _mbSchema name mbSub) Nothing)) =
      [AD.Pointer (nm : sub)]
      where
        nm = AD.OKey . AK.fromText . rAttrName $ name
        sub = [AD.OKey . AK.fromText . rAttrName $ subName | SubAttr subName <- maybeToList mbSub]
    mapPath (Just (ValuePath (AttrPath _mbSchema name Nothing) mbFilter)) =
      [AD.Pointer [nm]]
        <> case mbFilter of
          Nothing -> []
          Just fltr -> ixToValPaths <$> arrFilterToIndices fltr arr
      where
        nm@(AD.OKey key) = AD.OKey . AK.fromText . rAttrName $ name
        arr = case jsonOrig of
          Object obj -> case AK.lookup key obj of
            Just (Array vec) -> V.toList vec
            _ -> todo
          _ -> todo
        ixToValPaths :: Int -> AD.Pointer
        ixToValPaths ix = todo

arrFilterToIndices :: Filter -> [Value] -> [Int]
arrFilterToIndices _ _ = todo

-- | The inverse of `jsonPatchToScimPatch`.  This does not validate
-- schemas, and never fills the schema argument of `AttrPath`.  See
-- haddocks of `Patch` above.  Since `AD.Patch` is more expressive
-- than `Patch`, this can have errors.
jsonPatchToScimPatch :: forall tag m. (MonadError String m) => AD.Patch -> Value -> m (Patch tag)
jsonPatchToScimPatch jsonPatch jsonOrig = do
  (mapOp `mapM` (AD.patchOperations jsonPatch)) <&> Patch
  where
    mapOp :: AD.Operation -> m (PatchOp tag)
    mapOp = \case
      AD.Add path val -> (`PatchOpAdd` val) <$> mapPath path
      AD.Rem path -> mapPath path >>= maybe (throwError "remove op requires path argument.") (pure . PatchOpRemove)
      AD.Rep path val -> (`PatchOpReplace` val) <$> mapPath path
      AD.Mov {} -> throwError "unsupported patch operation: mov"
      AD.Cpy {} -> throwError "unsupported patch operation: cpy"
      AD.Tst {} -> throwError "unsupported patch operation: tst"

    mapPath :: AD.Pointer -> m (Maybe ValuePath)
    mapPath (AD.Pointer []) = pure Nothing
    mapPath (AD.Pointer [AD.OKey key]) = pure $ Just (ValuePath (topLevelAttrPath (AK.toText key)) Nothing)
    mapPath (AD.Pointer [AD.OKey key, AD.OKey sub]) = todo key sub
    mapPath (AD.Pointer [AD.OKey key, AD.AKey ix]) = do
      let fltr = arrIndexToFilter ix arr
          arr = case jsonOrig of
            Object obj -> case AK.lookup key obj of
              Just (Array vec) -> V.toList vec
              _ -> todo
            _ -> todo
          attr = topLevelAttrPath (AK.toText key)
      pure $ Just (ValuePath attr (Just fltr))
    mapPath (AD.Pointer [AD.OKey key, AD.AKey ix, AD.OKey sub]) = todo key ix sub
    mapPath bad = throwError $ "illegal or unsupported attribute path: " <> show bad

arrIndexToFilter :: Int -> [Value] -> Filter
arrIndexToFilter _ _ = todo

emptyPath :: AD.Pointer
emptyPath =
  parseEither AD.parsePointer ""
    & either (error . ("impossible: " <>) . show) Imports.id

----------------------------------------------------------------------

instance (SupportsSchemas tag) => ToJSON (Patch tag) where
  toJSON (Patch ops) =
    object $
      [ "schemas" .= [PatchOp20],
        "operations" .= ops
      ]

instance (SupportsSchemas tag) => ToJSON (PatchOp tag) where
  toJSON op =
    object $
      ["op" .= String (patchOpName op)]
        <> ["path" .= p | p <- maybeToList $ patchOpPath op]
        <> ["value" .= v | v <- maybeToList $ patchOpVal op]
    where
      patchOpName :: PatchOp tag -> Text
      patchOpName = \case
        PatchOpAdd _ _ -> "add"
        PatchOpRemove _ -> "remove"
        PatchOpReplace _ _ -> "replace"

      patchOpPath :: PatchOp tag -> Maybe ValuePath
      patchOpPath = \case
        PatchOpAdd mbp _ -> mbp
        PatchOpRemove p -> Just $ p
        PatchOpReplace mbp _ -> mbp

      patchOpVal :: PatchOp tag -> Maybe Value
      patchOpVal = \case
        PatchOpAdd _ v -> Just v
        PatchOpRemove _ -> Nothing
        PatchOpReplace _ v -> Just v

----------------------------------------------------------------------

instance (SupportsSchemas tag) => FromJSON (Patch tag) where
  parseJSON = prsJsonLower >=> prs
    where
      prs = withObject "ScimPatch" $ \ciObj -> do
        given <- ciObj .: "schemas"
        unless (given == Set.singleton PatchOp20) $ do
          fail $ "Unsupported schemas!  must be " <> show [getSchemaUri PatchOp20]
        Patch <$> ciObj .: "operations"

-- | Lower-case all case-insensitive parts of a scim value.  These are:
-- - Attributes schemas, operations, op of the patch itself (https://datatracker.ietf.org/doc/html/rfc7643#section-2.1)
-- - Attribute names in the values to be added / replaced (https://datatracker.ietf.org/doc/html/rfc7643#section-2.1)
-- - Attribute paths with filters (https://datatracker.ietf.org/doc/html/rfc7644#section-3.4.2.2)
--   (example: `filter=emails[type eq "work"] eq "john"` vs. `filter=EMAILS[TYPE EQ "WORK"] EQ "john"`)
lowerAllCaseInsensitiveThingsInPatch :: Value -> Either String Value
lowerAllCaseInsensitiveThingsInPatch = attrNamesInPaths <=< jsonLower
  where
    attrNamesInPaths = pure -- FUTUREWORK: we don't support this yet, so no need to lower-case it either.

instance (SupportsSchemas tag) => FromJSON (PatchOp tag) where
  parseJSON = (either fail pure . lowerAllCaseInsensitiveThingsInPatch) >=> prs
    where
      prs = withObject "ScimPatchOp" $ \o -> do
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

----------------------------------------------------------------------

-- Translate Patch into AD.Patch from the aeson-diff package and apply
-- the diff.  Validate input value and output value against supported
-- schemas, but validating the patch itself is redundant.
applyPatch ::
  forall m tag a.
  ( SupportsSchemas tag,
    FromJSON a,
    ToJSON a,
    MonadError ScimError m
  ) =>
  Patch tag ->
  a ->
  m a
applyPatch scimPatch (toJSON -> jsonOrig) = do
  let jsonPatch = scimPatchToJsonPatch scimPatch jsonOrig

      result err = \case
        Success val -> pure val
        Error txt -> throwError . badRequest InvalidValue . Just . err $ Text.pack txt

  jsonPatched <-
    AD.patch jsonPatch jsonOrig
      & result ("could not apply patch: " <>)

  validateSchemas @tag Proxy jsonOrig
    & either (throwError . badRequest InvalidSyntax . Just . ("Validation of input value failed: " <>) . Text.pack) pure
  validateSchemas @tag Proxy jsonPatched
    & either (throwError . badRequest InvalidSyntax . Just . ("Validation of output value failed: " <>) . Text.pack) pure

  fromJSON jsonPatched
    & result ("invalid patch result: " <>)
