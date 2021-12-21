{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

    -- * RichInfoMapAndList
    RichInfoMapAndList (richInfoMap, richInfoAssocList),
    mkRichInfoMapAndList,
    toRichInfoAssocList,
    fromRichInfoAssocList,

    -- * RichInfoAssocList
    RichInfoAssocList (unRichInfoAssocList),
    mkRichInfoAssocList,
    normalizeRichInfoAssocList,
    richInfoAssocListFromObject,
    richInfoAssocListURN,

    -- * RichField
    RichField (..),

    -- * Swagger
    modelRichInfo,
    modelRichField,
  )
where

import qualified Data.Schema as S
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary))

--------------------------------------------------------------------------------
-- RichInfo

-- | A 'RichInfoAssocList' that parses and renders as 'RichInfoMapAndList'.
newtype RichInfo = RichInfo {unRichInfo :: RichInfoAssocList}
  deriving stock (Eq, Show, Generic)

instance ToJSON RichInfo where
  toJSON = toJSON . fromRichInfoAssocList . unRichInfo

instance FromJSON RichInfo where
  parseJSON = fmap (RichInfo . toRichInfoAssocList) . parseJSON

instance Arbitrary RichInfo where
  arbitrary = RichInfo <$> arbitrary

instance Monoid RichInfo where
  mempty = RichInfo mempty

instance Semigroup RichInfo where
  RichInfo a <> RichInfo b = RichInfo $ a <> b

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
        (xs, (_ : ys)) -> xs <> [RichField key val] <> ys

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

-- | TODO: this is model is wrong, it says nothing about the map part.
modelRichInfo :: Doc.Model
modelRichInfo = Doc.defineModel "RichInfo" $ do
  Doc.description "Rich info about the user"
  Doc.property "fields" (Doc.array (Doc.ref modelRichField)) $
    Doc.description "List of fields"
  Doc.property "version" Doc.int32' $
    Doc.description "Format version (the current version is 0)"


instance ToJSON RichInfoMapAndList where
  toJSON u =
    object
      [ richInfoAssocListURN
          .= object
            [ "richInfo"
                .= object
                  [ "fields" .= richInfoAssocList u,
                    "version" .= (0 :: Int)
                  ]
            ],
        richInfoMapURN .= (Map.mapKeys CI.original $ richInfoMap u)
      ]

instance FromJSON RichInfoMapAndList where
  parseJSON =
    withObject "RichInfo" $
      \o ->
        let objWithCIKeys = hmMapKeys CI.mk o
         in normalizeRichInfoMapAndList
              <$> ( RichInfoMapAndList
                      <$> extractMap objWithCIKeys
                      <*> extractAssocList objWithCIKeys
                  )
    where
      extractMap :: HashMap (CI Text) Value -> Aeson.Parser (Map (CI Text) Text)
      extractMap o =
        case HM.lookup (CI.mk richInfoMapURN) o of
          Nothing -> pure mempty
          Just innerObj -> do
            Map.mapKeys CI.mk <$> parseJSON innerObj

      extractAssocList :: HashMap (CI Text) Value -> Aeson.Parser [RichField]
      extractAssocList o =
        case HM.lookup (CI.mk richInfoAssocListURN) o of
          Nothing -> pure []
          Just (Object innerObj) -> do
            richInfo <- lookupOrFail "richinfo" $ hmMapKeys CI.mk innerObj
            case richInfo of
              Object richinfoObj -> do
                fields <- richInfoAssocListFromObject richinfoObj
                pure fields
              Array fields -> parseJSON (Array fields)
              v -> Aeson.typeMismatch "Object or Array" v
          Just v -> Aeson.typeMismatch "Object" v

      hmMapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
      hmMapKeys f = HashMap.fromList . (map (\(k, v) -> (f k, v))) . HashMap.toList

      lookupOrFail :: (MonadFail m, Show k, Eq k, Hashable k) => k -> HashMap k v -> m v
      lookupOrFail key theMap = case HM.lookup key theMap of
        Nothing -> fail $ "key '" ++ show key ++ "' not found"
        Just v -> return v

instance Arbitrary RichInfoMapAndList where
  arbitrary = mkRichInfoMapAndList <$> arbitrary

-- | Uniform Resource Names used for serialization of 'RichInfo'.
richInfoMapURN, richInfoAssocListURN :: Text
richInfoMapURN = "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
richInfoAssocListURN = "urn:wire:scim:schemas:profile:1.0"

--------------------------------------------------------------------------------
-- RichInfoAssocList

newtype RichInfoAssocList = RichInfoAssocList {unRichInfoAssocList :: [RichField]}
  deriving stock (Eq, Show, Generic)

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

instance S.ToSchema RichInfoAssocList where
  schema = undefined


instance ToJSON RichInfoAssocList where
  toJSON (RichInfoAssocList l) =
    object
      [ "fields" .= l,
        "version" .= (0 :: Int)
      ]

instance FromJSON RichInfoAssocList where
  parseJSON v =
    mkRichInfoAssocList <$> withObject "RichInfoAssocList" richInfoAssocListFromObject v

richInfoAssocListFromObject :: Object -> Aeson.Parser [RichField]
richInfoAssocListFromObject richinfoObj = do
  version :: Int <- richinfoObj .: "version"
  when (version /= 0) $ fail $ "unknown version: " <> show version
  fields <- richinfoObj .: "fields"
  checkDuplicates (map richFieldType fields)
  pure fields
  where
    checkDuplicates :: [CI Text] -> Aeson.Parser ()
    checkDuplicates xs =
      case filter ((> 1) . length) . group . sort $ xs of
        [] -> pure ()
        ds -> fail ("duplicate fields: " <> show (map head ds))

instance Arbitrary RichInfoAssocList where
  arbitrary = mkRichInfoAssocList <$> arbitrary
  shrink (RichInfoAssocList things) = mkRichInfoAssocList <$> QC.shrink things

--------------------------------------------------------------------------------
-- RichField

data RichField = RichField
  { richFieldType :: CI Text,
    richFieldValue :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (S.Schema RichField)

modelRichField :: Doc.Model
modelRichField = Doc.defineModel "RichField" $ do
  Doc.description "RichInfo field"
  Doc.property "type" Doc.string' $
    Doc.description "Field name"
  Doc.property "value" Doc.string' $
    Doc.description "Field value"

instance Arbitrary RichField where
  arbitrary =
    RichField
      <$> (CI.mk . cs . QC.getPrintableString <$> arbitrary)
      <*> (cs . QC.getPrintableString <$> arbitrary)
  shrink (RichField k v) = RichField <$> QC.shrink k <*> QC.shrink v

instance S.ToSchema RichField where
  -- NB: "name" would be a better name for 'richFieldType', but "type" is used because we
  -- also have "type" in SCIM; and the reason we use "type" for SCIM is that @{"type": ...,
  -- "value": ...}@ is how all other SCIM payloads are formatted, so it's quite possible
  -- that some provisioning agent would support "type" but not "name".
  schema = S.object "RichField" $
    RichField
      <$> richFieldType  S..= S.field "type" (S.dimap CI.original CI.mk S.schema)
      <*> richFieldValue S..= S.field "value" S.schema

--------------------------------------------------------------------------------
-- convenience functions

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
--
-- NB: we could just calculate the length of JSON-encoded payload, but it is fragile because
-- if our JSON encoding changes, existing payloads might become unacceptable.
richInfoSize :: RichInfo -> Int
richInfoSize (RichInfo (RichInfoAssocList fields)) = sum [Text.length (CI.original t) + Text.length v | RichField t v <- fields]
