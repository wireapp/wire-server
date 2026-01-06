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
import qualified Data.CaseInsensitive as CI
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
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
          Object obj -> case lookupKeyCI key obj of
            Just (Array vec) -> V.toList vec
            _ -> todo
          _ -> todo
        ixToValPaths :: Int -> AD.Pointer
        ixToValPaths ix = todo
        lookupKeyCI :: AK.Key -> AK.KeyMap Value -> Maybe Value
        lookupKeyCI target obj =
          let target' = CI.foldCase (AK.toText target)
           in snd
                <$> find
                  (\(k, _) -> CI.foldCase (AK.toText k) == target')
                  (AK.toList obj)

arrFilterToIndices :: Filter -> [Value] -> [Int]
arrFilterToIndices filter arr =
  [ix | (ix, val) <- zip [0 ..] arr, matches val]
  where
    matches :: Value -> Bool
    matches val = case filter of
      FilterAttrCompare attr op compVal ->
        maybe False (compareValue op compVal) (attrValue attr val)

    attrValue :: AttrPath -> Value -> Maybe Value
    attrValue (AttrPath _ name mbSub) val = case mbSub of
      Nothing -> lookupAttr name val
      Just (SubAttr subName) -> do
        obj <- asObject val
        top <- lookupAttrInObject name obj
        subObj <- asObject top
        lookupAttrInObject subName subObj

    lookupAttr :: AttrName -> Value -> Maybe Value
    lookupAttr name val = case val of
      Object obj -> lookupAttrInObject name obj
      -- e.g. roles[value eq "admin"]
      _ | name == "value" -> Just val
      _ -> Nothing

    lookupAttrInObject :: AttrName -> AK.KeyMap Value -> Maybe Value
    lookupAttrInObject name obj =
      let target = CI.foldCase (rAttrName name)
       in snd <$> find (\(key, _) -> CI.foldCase (AK.toText key) == target) (AK.toList obj)

    asObject :: Value -> Maybe (AK.KeyMap Value)
    asObject = \case
      Object obj -> Just obj
      _ -> Nothing

    compareValue :: CompareOp -> CompValue -> Value -> Bool
    compareValue op compVal val = case (compVal, val) of
      (ValString s, String t) -> compareStr op (CI.foldCase t) (CI.foldCase s)
      (ValNumber s, Number t) -> compareNumber op t s
      (ValBool s, Bool t) -> compareBool op t s
      (ValNull, Null) -> compareNull op
      _ -> False

    compareNumber :: CompareOp -> Scientific -> Scientific -> Bool
    compareNumber = \case
      OpEq -> (==)
      OpNe -> (/=)
      OpGt -> (>)
      OpGe -> (>=)
      OpLt -> (<)
      OpLe -> (<=)
      OpCo -> \_ _ -> False
      OpSw -> \_ _ -> False
      OpEw -> \_ _ -> False

    compareBool :: CompareOp -> Bool -> Bool -> Bool
    compareBool op a b = case op of
      OpEq -> a == b
      OpNe -> a /= b
      _ -> False

    compareNull :: CompareOp -> Bool
    compareNull = \case
      OpEq -> True
      OpNe -> False
      _ -> False

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
arrIndexToFilter ix arr = case drop ix arr of
  [] -> todo
  (val : _) -> valToFilter val
  where
    valToFilter :: Value -> Filter
    valToFilter = \case
      Object obj -> objectToFilter obj
      other ->
        FilterAttrCompare
          (AttrPath Nothing "value" Nothing)
          OpEq
          (valueToCompValue other)

    objectToFilter :: AK.KeyMap Value -> Filter
    objectToFilter obj =
      case lookupPrimitiveKeyCI "value" obj <|> firstPrimitiveKey obj of
        Just (name, compVal) ->
          FilterAttrCompare (AttrPath Nothing name Nothing) OpEq compVal
        Nothing -> todo

    lookupPrimitiveKeyCI :: Text -> AK.KeyMap Value -> Maybe (AttrName, CompValue)
    lookupPrimitiveKeyCI target obj =
      let target' = CI.foldCase target
       in listToMaybe $
            mapMaybe
              ( \(k, v) ->
                  if CI.foldCase (AK.toText k) == target'
                    then (AttrName (AK.toText k),) <$> valueToCompValueMaybe v
                    else Nothing
              )
              (AK.toList obj)

    firstPrimitiveKey :: AK.KeyMap Value -> Maybe (AttrName, CompValue)
    firstPrimitiveKey obj =
      listToMaybe $
        mapMaybe
          (\(k, v) -> (AttrName (AK.toText k),) <$> valueToCompValueMaybe v)
          (AK.toList obj)

    valueToCompValue :: Value -> CompValue
    valueToCompValue val =
      fromMaybe todo (valueToCompValueMaybe val)

    valueToCompValueMaybe :: Value -> Maybe CompValue
    valueToCompValueMaybe = \case
      String s -> Just (ValString s)
      Number n -> Just (ValNumber n)
      Bool b -> Just (ValBool b)
      Null -> Just ValNull
      _ -> Nothing

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
