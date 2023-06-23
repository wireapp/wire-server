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
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as Map
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import Imports
import qualified Test.QuickCheck as QC
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
    object "RichInfo" $
      RichInfo . toRichInfoAssocList <$> (fromRichInfoAssocList . unRichInfo) .= richInfoMapAndListSchema

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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema RichInfoMapAndList

instance ToSchema RichInfoMapAndList where
  schema = object "RichInfoMapAndList" richInfoMapAndListSchema

richInfoMapAndListSchema :: ObjectSchema SwaggerDoc RichInfoMapAndList
richInfoMapAndListSchema =
  withParser
    ( RichInfoMapAndList
        <$> richInfoMap
          .= ( fromMaybe mempty
                 <$> optField richInfoMapURN richInfoMapSchema
             )
        <*> richInfoAssocList
          .= ( fromMaybe mempty
                 <$> optField
                   richInfoAssocListURN
                   ( unRichInfoAssocList
                       <$> object "RichInfoAssocList" (RichInfoAssocList .= field "richInfo" schema)
                   )
             )
    )
    (pure . normalizeRichInfoMapAndList)

richInfoMapSchema :: ValueSchema SwaggerDoc (Map (CI Text) Text)
richInfoMapSchema = mapWithKeys CI.original CI.mk schema

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
richInfoMapURN, richInfoAssocListURN :: Text
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
  schema = object "RichInfoAssocList" richInfoAssocListSchema

richInfoAssocListSchema :: ObjectSchema SwaggerDoc RichInfoAssocList
richInfoAssocListSchema =
  withParser
    ( (,)
        <$> const (0 :: Int) .= field "version" schema
        <*> unRichInfoAssocList .= field "fields" (array schema)
    )
    $ \(version, fields) ->
      mkRichInfoAssocList <$> validateRichInfoAssocList version fields

richInfoAssocListFromObject :: A.Object -> Aeson.Parser [RichField]
richInfoAssocListFromObject richinfoObj = do
  version :: Int <- richinfoObj A..: "version"
  fields <- richinfoObj A..: "fields"
  validateRichInfoAssocList version fields

validateRichInfoAssocList :: Int -> [RichField] -> Aeson.Parser [RichField]
validateRichInfoAssocList version fields = do
  when (version /= 0) $ fail $ "unknown version: " <> show version
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
      <$> (CI.mk . cs . QC.getPrintableString <$> arbitrary)
      <*> (cs . QC.getPrintableString <$> arbitrary)
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
