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
    toRichInfoAssocList,
    fromRichInfoAssocList,
    canonicalizeRichInfo,
    richInfoMapSize,
    normalizeRichInfo,
    richInfoMapURN,
    richInfoAssocListURN,

    -- * RichInfoAssocList
    RichInfoAssocList (..),
    emptyRichInfoAssocList,
    richInfoAssocListSize,
    normalizeRichInfoAssocList,

    -- * RichField
    RichField (..),

    -- * Swagger
    modelRichInfo,
    modelRichField,
  )
where

import Control.Monad.Fail (MonadFail)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List.Extra (nubOn)
import qualified Data.Map as Map
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as Text
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary))

--------------------------------------------------------------------------------
-- RichInfo

-- | Represents all the ways we can recieve 'RichInfo' from a SCIM peer.  'richInfoMap'
-- represents fields given under 'richInfoMapURN' as a JSON object.  'richInfoAssocList'
-- represents fields given under 'richInfoAssocListURN' as an assoc list.  Use the much
-- simpler 'RichInfoAssocList' if you can.  Use 'toRichInfoAssocList' to turn 'RichInfo' into
-- 'RichInfoAssocList'.
data RichInfo = RichInfo
  { richInfoMap :: Map (CI Text) Text,
    richInfoAssocList :: [RichField]
  }
  deriving stock (Eq, Show, Generic)

modelRichInfo :: Doc.Model
modelRichInfo = Doc.defineModel "RichInfo" $ do
  Doc.description "Rich info about the user"
  Doc.property "fields" (Doc.array (Doc.ref modelRichField)) $
    Doc.description "List of fields"
  Doc.property "version" Doc.int32' $
    Doc.description "Format version (the current version is 0)"

instance ToJSON RichInfo where
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

instance FromJSON RichInfo where
  parseJSON =
    withObject "RichInfo" $
      \o ->
        let objWithCIKeys = hmMapKeys CI.mk o
         in normalizeRichInfo
              <$> ( RichInfo
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
              v -> Aeson.typeMismatch "Object" v
          Just v -> Aeson.typeMismatch "Object" v
      hmMapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
      hmMapKeys f = HashMap.fromList . (map (\(k, v) -> (f k, v))) . HashMap.toList
      lookupOrFail :: (MonadFail m, Show k, Eq k, Hashable k) => k -> HashMap k v -> m v
      lookupOrFail key theMap = case HM.lookup key theMap of
        Nothing -> fail $ "key '" ++ show key ++ "' not found"
        Just v -> return v

instance Arbitrary RichInfo where
  arbitrary = do
    RichInfoAssocList richInfoAssocList <- arbitrary
    richInfoMap <- arbitrary
    pure RichInfo {..}

-- | Lossy transformation of map-and-list representation into list-only representation.  The
-- order of the list part of 'RichInfo' is not changed in the output; keys in the map that do
-- not appear in the list are appended in alpha order.
toRichInfoAssocList :: RichInfo -> RichInfoAssocList
toRichInfoAssocList (RichInfo mp al) =
  RichInfoAssocList $ foldl' go al (Map.toAscList mp)
  where
    go :: [RichField] -> (CI Text, Text) -> [RichField]
    go rfs (key, val) =
      case break (\(RichField rfKey _) -> rfKey == key) rfs of
        (xs, []) -> xs <> [RichField key val]
        (xs, (_ : ys)) -> xs <> [RichField key val] <> ys

fromRichInfoAssocList :: RichInfoAssocList -> RichInfo
fromRichInfoAssocList (RichInfoAssocList riList) = RichInfo riMap riList
  where
    riMap = Map.fromList $ map (\(RichField key value) -> (key, value)) riList

-- | Mostly for testing: If you construct an arbitrary valid 'RichInfo' value and pass it to
-- spar, 'canonicalizeRichInfo' can tell you what it will look like when you get it back.  See also:
-- 'normalizeRichInfo'.
canonicalizeRichInfo :: RichInfo -> RichInfo
canonicalizeRichInfo = fromRichInfoAssocList . toRichInfoAssocList

-- | Uniform Resource Names used for serialization of 'RichInfo'.
richInfoMapURN, richInfoAssocListURN :: Text
richInfoMapURN = "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
richInfoAssocListURN = "urn:wire:scim:schemas:profile:1.0"

--------------------------------------------------------------------------------
-- RichInfoAssocList

newtype RichInfoAssocList = RichInfoAssocList [RichField]
  deriving stock (Eq, Show, Generic)

instance ToJSON RichInfoAssocList where
  toJSON (RichInfoAssocList l) =
    object
      [ "fields" .= l,
        "version" .= (0 :: Int)
      ]

instance FromJSON RichInfoAssocList where
  parseJSON v =
    RichInfoAssocList <$> withObject "RichInfoAssocList" richInfoAssocListFromObject v

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
  arbitrary = RichInfoAssocList <$> nubOn richFieldType <$> arbitrary

emptyRichInfoAssocList :: RichInfoAssocList
emptyRichInfoAssocList = RichInfoAssocList []

--------------------------------------------------------------------------------
-- RichField

data RichField = RichField
  { richFieldType :: CI Text,
    richFieldValue :: Text
  }
  deriving stock (Eq, Show, Generic)

modelRichField :: Doc.Model
modelRichField = Doc.defineModel "RichField" $ do
  Doc.description "RichInfo field"
  Doc.property "type" Doc.string' $
    Doc.description "Field name"
  Doc.property "value" Doc.string' $
    Doc.description "Field value"

instance ToJSON RichField where
  -- NB: "name" would be a better name for 'richFieldType', but "type" is used because we
  -- also have "type" in SCIM; and the reason we use "type" for SCIM is that @{"type": ...,
  -- "value": ...}@ is how all other SCIM payloads are formatted, so it's quite possible
  -- that some provisioning agent would support "type" but not "name".
  toJSON u =
    object
      [ "type" .= CI.original (richFieldType u),
        "value" .= richFieldValue u
      ]

instance FromJSON RichField where
  parseJSON = withObject "RichField" $ \o -> do
    RichField
      <$> (CI.mk <$> o .: "type")
      <*> o .: "value"

instance Arbitrary RichField where
  arbitrary =
    RichField
      <$> arbitrary
      <*> (arbitrary `QC.suchThat` (/= "")) -- This is required because FromJSON calls @normalizeRichInfo@ and roundtrip tests fail

--------------------------------------------------------------------------------
-- convenience functions

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
--
-- This works on @[RichField]@ so it can be reused by @RichInfoAssocList@
--
-- NB: we could just calculate the length of JSON-encoded payload, but it is fragile because
-- if our JSON encoding changes, existing payloads might become unacceptable.
richInfoAssocListSize :: [RichField] -> Int
richInfoAssocListSize fields = sum [Text.length (CI.original t) + Text.length v | RichField t v <- fields]

-- | Calculate the length of user-supplied data in 'RichInfo'. Used for enforcing
-- 'setRichInfoLimit'
richInfoMapSize :: RichInfo -> Int
richInfoMapSize rif = sum [Text.length (CI.original k) + Text.length v | (k, v) <- Map.toList $ richInfoMap rif]

-- | Remove fields with @""@ values.  See also: 'canonicalizeRichInfo'.
normalizeRichInfo :: RichInfo -> RichInfo
normalizeRichInfo (RichInfo rifMap assocList) =
  RichInfo
    { richInfoAssocList = filter (not . Text.null . richFieldValue) assocList,
      richInfoMap = rifMap
    }

-- | Remove fields with @""@ values.
normalizeRichInfoAssocList :: RichInfoAssocList -> RichInfoAssocList
normalizeRichInfoAssocList (RichInfoAssocList l) =
  RichInfoAssocList $ filter (not . Text.null . richFieldValue) l
