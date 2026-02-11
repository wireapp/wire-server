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

import Control.Monad.Error.Class
import Data.Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AK
import qualified Data.Aeson.Patch as AD
import qualified Data.Aeson.Pointer as AD
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
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

-- | Check whether a pointer exists in a given Value.
pointerExists :: AD.Pointer -> Value -> Bool
pointerExists (AD.Pointer keys) jsonVal = go keys jsonVal
  where
    go :: [AD.Key] -> Value -> Bool
    go [] _ = True
    go (k : ks) v = case k of
      AD.OKey key -> case v of
        Object obj ->
          case AK.lookup key obj of
            Just next -> go ks next
            Nothing -> False
        _ -> False
      AD.AKey ix -> case v of
        Array vec ->
          let len = V.length vec
           in if ix >= 0 && ix < len
                then go ks (vec V.! ix)
                else False
        _ -> False

-- | Compute a list of patch operations for the aeson-diff package.  The
-- `Value` argument is needed to compute absolute indices into arrays
-- from the filter expressions in the scim patch.
--
-- Scim schema information in `AttrName`s is ignored (`AD.Patch` does
-- not do schema validation).
scimPatchToJsonPatch :: forall tag m. (MonadError ScimError m) => Patch tag -> Value -> m [AD.Operation]
scimPatchToJsonPatch (Patch scimOps) jsonOrig = do
  ops <- mapM mapOp scimOps
  pure $ concat ops
  where
    mapOp :: PatchOp tag -> m [AD.Operation]
    mapOp = \case
      PatchOpAdd mbAttrPath val ->
        case mapPath mbAttrPath of
          Left e -> throwError $ badRequest InvalidValue (Just $ Text.pack e)
          Right ptr -> pure $ ensurePrefixes ptr ++ [AD.Add ptr val]
      PatchOpRemove attrPath -> do
        checkMutability attrPath
        case mapPath (Just attrPath) of
          Left e -> throwError $ badRequest InvalidValue (Just $ Text.pack e)
          Right ptr ->
            if pointerExists ptr jsonOrig
              then pure [AD.Rem ptr]
              else pure [] -- Ignore remove operations on non-existent paths
      PatchOpReplace mbAttrPath val ->
        case mapPath mbAttrPath of
          Left e -> throwError $ badRequest InvalidValue (Just $ Text.pack e)
          Right ptr@(AD.Pointer keys) ->
            if any isAKey keys
              then pure [AD.Rep ptr val] -- Use replace for array keys
              else pure $ ensurePrefixes ptr ++ [AD.Add ptr val] -- Use add (upsert) for object keys
    checkMutability :: ValuePath -> m ()
    checkMutability (ValuePath (AttrPath _ name _) _)
      | CI.foldCase (rAttrName name) == "username" =
          throwError $ badRequest Mutability Nothing
      | otherwise = pure ()

    ensurePrefixes :: AD.Pointer -> [AD.Operation]
    ensurePrefixes (AD.Pointer keys) =
      [ AD.Add (AD.Pointer p) (Object mempty)
      | i <- [1 .. length keys - 1],
        let p = take i keys,
        not (pointerExists (AD.Pointer p) jsonOrig),
        case last p of AD.OKey _ -> True; _ -> False
      ]

    isAKey :: AD.Key -> Bool
    isAKey AD.AKey {} = True
    isAKey _ = False

    mapPath :: Maybe ValuePath -> Either String AD.Pointer
    mapPath Nothing = pure emptyPath
    mapPath (Just (ValuePath (AttrPath mbSchema name mbSub) mbFilter)) =
      case (mbSub, mbFilter) of
        (sub, Nothing) ->
          let path = [AD.OKey . AK.fromText . CI.foldCase . rAttrName $ subName | SubAttr subName <- maybeToList sub]
              base = AD.OKey (AK.fromText . CI.foldCase . rAttrName $ name)
              fullPath = case mbSchema of
                Just s | s /= User20 -> AD.OKey (AK.fromText . CI.foldCase . getSchemaUri $ s) : base : path
                _ -> base : path
           in pure (AD.Pointer fullPath)
        (Nothing, Just fl) ->
          let keyText = CI.foldCase (rAttrName name)
              key = AK.fromText keyText
              nm = AD.OKey key
           in AD.Pointer <$> ((nm :) <$> fltr fl)
        _ -> throwError $ "scimPatchToJsonPatch: illegal or unsupported attribute path: " <> show (ValuePath (AttrPath mbSchema name mbSub) mbFilter)
      where
        fltr :: Filter -> Either String [AD.Key]
        fltr fl = do
          let keyText = CI.foldCase (rAttrName name)
              key = AK.fromText keyText
          arr <- case jsonOrig of
            Object obj -> case AK.lookup key obj of
              Just (Array vec) -> pure $ V.toList vec
              _ -> throwError $ AK.toString key <> " does not point to an array"
            _ -> throwError "not an object"
          let mkPointer ix = AD.AKey ix
          pure $ mkPointer <$> arrFilterToIndices fl arr

arrFilterToIndices :: Filter -> [Value] -> [Int]
arrFilterToIndices fltr arr =
  [ix | (ix, val) <- zip [0 ..] arr, matches val]
  where
    matches :: Value -> Bool
    matches val = case fltr of
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
jsonPatchToScimPatch :: forall tag m. (MonadError ScimError m) => AD.Patch -> Value -> m (Patch tag)
jsonPatchToScimPatch jsonPatch jsonOrig = do
  case traverse mapOp (AD.patchOperations jsonPatch) of
    Left e -> throwError . badRequest InvalidValue . Just . Text.pack $ e
    Right ops -> pure $ Patch $ concat ops
  where
    mapOp :: AD.Operation -> Either String [PatchOp tag]
    mapOp = \case
      AD.Add path val ->
        -- Add becomes Replace if target exists
        if pointerExists path jsonOrig
          then Right [PatchOpReplace Nothing val] -- Use replace semantics if target exists
          else Right [PatchOpAdd Nothing val]
      AD.Rem path ->
        case mapPath path of
          Left _ -> Right []
          Right Nothing -> Right []
          Right (Just p) -> Right [PatchOpRemove p]
      AD.Rep path val ->
        case mapPath path of
          Left _ -> Right []
          Right mbp -> Right [PatchOpReplace mbp val]
      AD.Mov {} -> Left "unsupported patch operation: mov"
      AD.Cpy {} -> Left "unsupported patch operation: cpy"
      AD.Tst {} -> Left "unsupported patch operation: tst"

    mapPath :: AD.Pointer -> Either String (Maybe ValuePath)
    mapPath (AD.Pointer []) = pure Nothing
    mapPath (AD.Pointer [AD.OKey key]) = pure $ Just (ValuePath (topLevelAttrPath (AK.toText key)) Nothing)
    mapPath (AD.Pointer [AD.OKey key, AD.OKey sub]) =
      pure $ Just (ValuePath (AttrPath Nothing (AttrName (AK.toText key)) (Just (SubAttr (AttrName (AK.toText sub))))) Nothing)
    mapPath (AD.Pointer [AD.OKey key, AD.AKey ix]) = do
      arr <- case jsonOrig of
        Object obj -> case AK.lookup key obj of
          Just (Array vec) -> pure $ V.toList vec
          _ -> throwError $ AK.toString key <> " does not point to an array"
        _ -> throwError "not an object"
      let fltr = arrIndexToFilter ix arr
          attr = topLevelAttrPath (AK.toText key)
      pure $ Just (ValuePath attr (Just fltr))
    mapPath (AD.Pointer [AD.OKey key, AD.AKey ix, AD.OKey subKey]) = do
      -- Handle nested path [key, ix, subkey] by resolving filter
      arr <- case jsonOrig of
        Object obj -> case AK.lookup key obj of
          Just (Array vec) -> pure $ V.toList vec
          _ -> throwError $ AK.toString key <> " does not point to an array"
        _ -> throwError "not an object"
      if ix < 0 || ix >= length arr
        then throwError $ "jsonPatchToScimPatch: index out of bounds: " <> show (key, ix, subKey)
        else do
          let fltr = arrIndexToFilter ix arr
              subAttr = SubAttr (AttrName (AK.toText subKey))
          pure $ Just (ValuePath (AttrPath Nothing (AttrName (AK.toText key)) (Just subAttr)) (Just fltr))
    mapPath bad = do
      throwError $ "jsonPatchToScimPatch: illegal or unsupported attribute path: " <> show bad

arrIndexToFilter :: Int -> [Value] -> Filter
arrIndexToFilter ix arr = case drop ix arr of
  [] -> error "arrIndexToFilter: index out of bounds"
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
        Nothing -> error "objectToFilter: no primitive key found"

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
      fromMaybe (error "valueToCompValue: not a primitive value") (valueToCompValueMaybe val)

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

instance ToJSON (Patch tag) where
  toJSON (Patch ops) =
    object $
      [ "schemas" .= [PatchOp20],
        "operations" .= ops
      ]

instance ToJSON (PatchOp tag) where
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

instance FromJSON (Patch tag) where
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

instance FromJSON (PatchOp tag) where
  parseJSON = (either fail pure . lowerAllCaseInsensitiveThingsInPatch) >=> prs
    where
      prs = withObject "ScimPatchOp" $ \o -> do
        o .: "op" >>= \(T.toLower -> op) -> case op of
          "add" -> do
            path <- o .:? "path"
            val <- o .: "value"
            pure $ PatchOpAdd path val
          "remove" -> do
            path <- o .:? "path"
            case path of
              Just p -> pure $ PatchOpRemove p
              Nothing -> fail "noTarget"
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
  ( FromJSON a,
    ToJSON a,
    MonadError ScimError m
  ) =>
  Patch tag ->
  a ->
  m a
applyPatch scimPatch (toJSON -> jsonOrig) = do
  let result err = \case
        Success val -> pure val
        Error txt -> throwError . badRequest InvalidValue . Just . err $ Text.pack txt

  jsonOrigLower <- case jsonLower jsonOrig of
    Left err -> throwError . badRequest InvalidValue . Just . Text.pack $ err
    Right val -> pure val

  jsonOps <- scimPatchToJsonPatch scimPatch jsonOrigLower

  jsonPatched <-
    AD.patch (AD.Patch jsonOps) jsonOrigLower
      & result ("could not apply patch: " <>)

  fromJSON jsonPatched
    & result ("invalid patch result: " <>)
