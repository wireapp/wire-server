{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.RichInfo
  ( -- * RichInfo
    RichInfo (..),
    richInfoSize,
    richInfoMapURN,
    mkRichInfo,

    -- * RichInfoMapAndList
    RichInfoMapAndList (richInfoMap, richInfoAssocList),
    mkRichInfoMapAndList,
    toRichInfoAssocList,
    fromRichInfoAssocList,

    -- * RichInfoAssocList
    RichInfoAssocList (unRichInfoAssocList),
    mkRichInfoAssocList,
    normalizeRichInfoAssocList,
    richInfoAssocListURN,

    -- * RichField
    RichField (..),
  )
where

import Cassandra qualified as C
import Control.Lens ((%~), (?~), _1)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Types qualified as A
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.List.Extra (nubOrdOn)
import Data.Map qualified as Map
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Imports
import Test.QuickCheck qualified as QC
import Wire.Arbitrary (Arbitrary (arbitrary))

--------------------------------------------------------------------------------
-- RichInfo

-- | A 'RichInfoAssocList' that parses and renders as 'RichInfoMapAndList'.
newtype RichInfo = RichInfo {unRichInfo :: RichInfoAssocList}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema RichInfo

instance ToSchema RichInfo where
  schema =
    ciObject "RichInfo" $
      RichInfo . toRichInfoAssocList <$> (fromRichInfoAssocList . unRichInfo) .= richInfoMapAndListSchema

instance Monoid RichInfo where
  mempty = RichInfo mempty

instance Semigroup RichInfo where
  RichInfo a <> RichInfo b = RichInfo $ a <> b

mkRichInfo :: [RichField] -> RichInfo
mkRichInfo = RichInfo . normalizeRichInfoAssocList . RichInfoAssocList

--------------------------------------------------------------------------------
-- RichInfoMapAndList

-- | Represents all the ways we can recieve 'RichInfo' from a SCIM peer.
--
-- 'richInfoMap' represents fields given under 'richInfoMapURN' as a JSON object (use case:
-- enterprise extensions as sent by eg. microsoft azure).
--
-- 'richInfoAssocList' represents fields given under 'richInfoAssocListURN' as an assoc list
-- (use case: wire native code; we used this so we can give the client arbitrary order in
-- which to show the rich info).
--
-- Internally we only store one assoc list.  This type is just to keep serialization separate
-- from the tricky semantics of how the map is merged into the list.  See
-- 'toRichInfoAssocList', 'fromRichInfoAssocList' for the merge semantics.
--
-- TODO: https://github.com/zinfra/backend-issues/issues/1627
data RichInfoMapAndList = RichInfoMapAndList
  { richInfoMap :: Map (CI Text) Text,
    richInfoAssocList :: [RichField]
  }
  deriving stock (Eq, Show, Generic)

-- | 'CIObjectSchema' is a bit of a hack, so it is not included in schema-profunctor even
-- though it is pretty general.
--
-- [scim]{https://www.rfc-editor.org/rfc/rfc7644} requires case insensitivity in json object
-- field names.  while this violates the json standard, it is necessary to follow this
-- requirement in order to be interoperable.  for this purpose, 'CIObjectSchema' supports `Map
-- (CI Text) Value` in place of `A.Object`.  only use when you know what you're doing!
type CIObjectSchema doc a = CIObjectSchemaP doc a a

-- | See CIObjectSchema
type CIObjectSchemaP doc = SchemaP doc (Map (CI Text) A.Value) [(CI Text, A.Value)]

-- | See 'CIObjectSchema'.
ciObject ::
  forall doc doc' a b.
  (HasObject doc doc', S.HasDescription doc' (Maybe Text)) =>
  Text ->
  CIObjectSchemaP doc a b ->
  ValueSchemaP doc' a b
ciObject name sch = mkSchema s r w
  where
    s :: doc'
    s = mkObject name (schemaDoc sch) & desc
      where
        desc = S.description ?~ ("json object with case-insensitive fields." :: Text)

    r :: A.Value -> A.Parser b
    r = A.withObject (Text.unpack name) f
      where
        f :: A.Object -> A.Parser b
        f = schemaIn sch . g

        g :: A.Object -> Map (CI Text) A.Value
        g = Map.fromList . fmap (_1 %~ (CI.mk . A.toText)) . A.toList

    w :: a -> Maybe A.Value
    w = fmap (A.object . f) . schemaOut sch
      where
        f :: [(CI Text, A.Value)] -> [A.Pair]
        f = fmap (\(k, v) -> A.fromText (CI.original k) A..= v)

-- | See 'CIObjectSchema'.
ciField ::
  forall doc doc' a.
  (HasField doc doc', S.HasDescription doc' (Maybe Text)) =>
  CI Text ->
  ValueSchema doc a ->
  CIObjectSchema doc' a
ciField name sch = mkSchema s r w
  where
    s :: doc'
    s = mkDocF @doc' @Identity (mkField (CI.original name) (schemaDoc sch)) & desc
      where
        desc = S.description ?~ ("json field with case-insensitive keys." :: Text)

    r :: Map (CI Text) A.Value -> A.Parser a
    r = maybe (fail $ "missing object field " <> show name) (schemaIn sch) . Map.lookup name

    w :: a -> Maybe [(CI Text, A.Value)]
    w = fmap ((: []) . (name,)) . schemaOut sch

-- | See 'CIObjectSchema'.
ciOptField ::
  forall doc' doc a.
  (HasField doc' doc, S.HasDescription doc (Maybe Text)) =>
  CI Text ->
  ValueSchema doc' a ->
  CIObjectSchemaP doc a (Maybe a)
ciOptField name sch = mkSchema s r w
  where
    s :: doc
    s = mkDocF @doc @Identity (mkField (CI.original name) (schemaDoc sch)) & desc
      where
        desc = S.description ?~ ("optional json field with case-insensitive keys." :: Text)

    r :: Map (CI Text) A.Value -> A.Parser (Maybe a)
    r obj = case Map.lookup name obj of
      Nothing -> pure Nothing
      Just a -> fmap Just (schemaIn sch a)

    w :: a -> Maybe [(CI Text, A.Value)]
    w = fmap ((: []) . (name,)) . schemaOut sch

richInfoMapAndListSchema :: CIObjectSchema SwaggerDoc RichInfoMapAndList
richInfoMapAndListSchema =
  withParser
    ( RichInfoMapAndList
        <$> richInfoMap
          .= ( fromMaybe mempty
                 <$> ciOptField richInfoMapURN (mapWithKeys CI.original CI.mk schema)
             )
        <*> richInfoAssocList
          .= ( fromMaybe mempty
                 <$> ciOptField
                   richInfoAssocListURN
                   ( unRichInfoAssocList
                       <$> ciObject
                         "RichInfoAssocList"
                         ( RichInfoAssocList
                             .= ciField
                               "richInfo"
                               (unnamed schema <> richInfoAssocListSchemaLegacy)
                         )
                   )
             )
    )
    (pure . normalizeRichInfoMapAndList)
  where
    richInfoAssocListSchemaLegacy :: ValueSchema SwaggerDoc RichInfoAssocList
    richInfoAssocListSchemaLegacy = RichInfoAssocList <$> unRichInfoAssocList .= array (schema @RichField)

-- | Uses 'normalizeRichInfoMapAndList'.
mkRichInfoMapAndList :: [RichField] -> RichInfoMapAndList
mkRichInfoMapAndList = normalizeRichInfoMapAndList . RichInfoMapAndList mempty

-- | Remove fields with @""@ values; make both map and assoc list contain the union of their
-- data; handle case insensitivity.  See also: 'normalizeRichInfo'.
normalizeRichInfoMapAndList :: RichInfoMapAndList -> RichInfoMapAndList
normalizeRichInfoMapAndList = fromRichInfoAssocList . toRichInfoAssocList

-- | Lossy transformation of map-and-list representation into list-only representation.  The
-- order of the list part of 'RichInfo' is not changed in the output; keys in the map that do
-- not appear in the list are appended in alpha order.
--
-- Uses 'mkRichInfoAssocList'; used as one half of 'normalizeRichInfoAssocList'.
toRichInfoAssocList :: RichInfoMapAndList -> RichInfoAssocList
toRichInfoAssocList (RichInfoMapAndList mp al) =
  mkRichInfoAssocList $ foldl' go al (Map.toAscList mp)
  where
    go :: [RichField] -> (CI Text, Text) -> [RichField]
    go rfs (key, val) =
      case break (\(RichField rfKey _) -> rfKey == key) rfs of
        (xs, []) -> xs <> [RichField key val]
        (xs, _ : ys) -> xs <> [RichField key val] <> ys

-- | This is called by spar to recover the more type that also contains a map.  Since we don't
-- know where the data came from when it was posted or where the SCIM peer expects the data to
-- be (map or assoc list), we copy the assoc list into the map, and provide all attributes
-- twice.
--
-- Used as the other half of 'normalizeRichInfoAssocList' (next to 'toRichInfoAssocList'.
fromRichInfoAssocList :: RichInfoAssocList -> RichInfoMapAndList
fromRichInfoAssocList (RichInfoAssocList riList) =
  RichInfoMapAndList
    { richInfoMap = riMap,
      richInfoAssocList = riList'
    }
  where
    riList' = normalizeRichInfoAssocListInt riList
    riMap = Map.fromList $ (\(RichField k v) -> (k, v)) <$> riList'

instance Arbitrary RichInfoMapAndList where
  arbitrary = mkRichInfoMapAndList <$> arbitrary

-- | Uniform Resource Names used for serialization of 'RichInfo'.
richInfoMapURN, richInfoAssocListURN :: (IsString s) => s
richInfoMapURN = "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
richInfoAssocListURN = "urn:wire:scim:schemas:profile:1.0"

--------------------------------------------------------------------------------
-- RichInfoAssocList

newtype RichInfoAssocList = RichInfoAssocList {unRichInfoAssocList :: [RichField]}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RichInfoAssocList)

-- | Uses 'normalizeRichInfoAssocList'.
mkRichInfoAssocList :: [RichField] -> RichInfoAssocList
mkRichInfoAssocList = RichInfoAssocList . normalizeRichInfoAssocListInt

normalizeRichInfoAssocList :: RichInfoAssocList -> RichInfoAssocList
normalizeRichInfoAssocList = RichInfoAssocList . normalizeRichInfoAssocListInt . unRichInfoAssocList

normalizeRichInfoAssocListInt :: [RichField] -> [RichField]
normalizeRichInfoAssocListInt = nubOrdOn nubber . filter ((/= mempty) . richFieldValue)
  where
    -- see also: https://github.com/basvandijk/case-insensitive/issues/31
    nubber = Text.toLower . Text.toCaseFold . CI.foldedCase . richFieldType

instance Monoid RichInfoAssocList where
  mempty = RichInfoAssocList mempty

instance Semigroup RichInfoAssocList where
  RichInfoAssocList a <> RichInfoAssocList b = RichInfoAssocList $ a <> b

instance ToSchema RichInfoAssocList where
  schema = ciObject "RichInfoAssocList" richInfoAssocListSchema
    where
      richInfoAssocListSchema :: CIObjectSchema SwaggerDoc RichInfoAssocList
      richInfoAssocListSchema =
        withParser
          ( (,)
              <$> const (0 :: Int) .= ciField "version" schema
              <*> unRichInfoAssocList .= ciField "fields" (array schema)
          )
          $ \(version, fields) ->
            mkRichInfoAssocList <$> validateRichInfoAssocList version fields

validateRichInfoAssocList :: Int -> [RichField] -> A.Parser [RichField]
validateRichInfoAssocList version fields = do
  when (version /= 0) $ fail $ "unknown version: " <> show version
  checkDuplicates (map richFieldType fields)
  pure fields
  where
    checkDuplicates :: [CI Text] -> A.Parser ()
    checkDuplicates xs =
      case filter ((> 1) . length) . group . sort $ xs of
        [] -> pure ()
        ds -> fail ("duplicate fields: " <> show (map head ds))

instance Arbitrary RichInfoAssocList where
  arbitrary = mkRichInfoAssocList <$> arbitrary
  shrink (RichInfoAssocList things) = mkRichInfoAssocList <$> QC.shrink things

instance C.Cql RichInfoAssocList where
  ctype = C.Tagged C.BlobColumn
  toCql = C.toCql . C.Blob . A.encode
  fromCql (C.CqlBlob v) = A.eitherDecode v
  fromCql _ = Left "RichInfo: Blob expected"

--------------------------------------------------------------------------------
-- RichField

data RichField = RichField
  { richFieldType :: CI Text,
    richFieldValue :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RichField)

instance ToSchema RichField where
  -- NB: "name" would be a better name for 'richFieldType', but "type" is used because we
  -- also have "type" in SCIM; and the reason we use "type" for SCIM is that @{"type": ...,
  -- "value": ...}@ is how all other SCIM payloads are formatted, so it's quite possible
  -- that some provisioning agent would support "type" but not "name".
  schema =
    object "RichField" $
      RichField
        <$> richFieldType .= field "type" (CI.original .= (CI.mk <$> schema))
        <*> richFieldValue .= field "value" schema

instance Arbitrary RichField where
  arbitrary =
    RichField
      <$> (CI.mk . Text.pack . QC.getPrintableString <$> arbitrary)
      <*> (Text.pack . QC.getPrintableString <$> arbitrary)
  shrink (RichField k v) = RichField <$> QC.shrink k <*> QC.shrink v

--------------------------------------------------------------------------------
-- convenience functions

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
--
-- NB: we could just calculate the length of JSON-encoded payload, but it is fragile because
-- if our JSON encoding changes, existing payloads might become unacceptable.
richInfoSize :: RichInfo -> Int
richInfoSize (RichInfo (RichInfoAssocList fields)) = sum [Text.length (CI.original t) + Text.length v | RichField t v <- fields]
