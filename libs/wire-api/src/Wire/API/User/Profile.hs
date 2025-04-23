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

module Wire.API.User.Profile
  ( Name (..),
    mkName,
    TextStatus,
    mkTextStatus,
    fromTextStatus,
    ColourId (..),
    defaultAccentId,

    -- * Asset
    Asset (..),
    AssetSize (..),

    -- * ManagedBy
    ManagedBy (..),
    defaultManagedBy,
    managedByToInt32,
    managedByFromInt32,

    -- * Deprecated
    Pict (..),
    noPict,
  )
where

import Cassandra qualified as C
import Control.Error (note)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString.Char8 (takeByteString)
import Data.ByteString.Conversion
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Data.Text qualified as Text
import Imports
import Wire.API.Asset (AssetKey (..))
import Wire.API.User.Orphans ()
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Name

-- | Usually called display name.
-- Length is between 1 and 128 characters.
newtype Name = Name
  {fromName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromByteString, ToByteString)
  deriving (Arbitrary) via (Ranged 1 128 Text)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Name

mkName :: Text -> Either String Name
mkName txt = Name . fromRange <$> checkedEitherMsg @_ @1 @128 "Name" txt

instance ToSchema Name where
  schema = Name <$> fromName .= untypedRangedSchema 1 128 schema

deriving instance C.Cql Name

--------------------------------------------------------------------------------
-- TextStatus

-- Length is between 1 and 256 characters.
newtype TextStatus = TextStatus
  {fromTextStatus :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromByteString, ToByteString)
  deriving (Arbitrary) via (Ranged 1 256 Text)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema TextStatus

mkTextStatus :: Text -> Either String TextStatus
mkTextStatus txt = TextStatus . fromRange <$> checkedEitherMsg @_ @1 @256 "TextStatus" txt

instance ToSchema TextStatus where
  schema = TextStatus <$> fromTextStatus .= untypedRangedSchema 1 256 schema

deriving instance C.Cql TextStatus

--------------------------------------------------------------------------------
-- Colour

newtype ColourId = ColourId {fromColourId :: Int32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToSchema, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ColourId

defaultAccentId :: ColourId
defaultAccentId = ColourId 0

deriving instance C.Cql ColourId

--------------------------------------------------------------------------------
-- Asset

-- Note: Intended to be turned into a sum type to add further asset types.
data Asset = ImageAsset
  { assetKey :: AssetKey,
    assetSize :: Maybe AssetSize
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Asset)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Asset

instance ToSchema Asset where
  schema =
    object "UserAsset" $
      ImageAsset
        <$> assetKey .= field "key" schema
        <*> assetSize .= maybe_ (optField "size" schema)
        <* const () .= field "type" typeSchema
    where
      typeSchema :: ValueSchema NamedSwaggerDoc ()
      typeSchema =
        enum @Text @NamedSwaggerDoc "AssetType" $
          element "image" ()

instance C.Cql Asset where
  -- Note: Type name and column names and types must match up with the
  --       Cassandra schema definition. New fields may only be added
  --       (appended) but no fields may be removed.
  ctype =
    C.Tagged
      ( C.UdtColumn
          "asset"
          [ ("typ", C.IntColumn),
            ("key", C.TextColumn),
            ("size", C.MaybeColumn C.IntColumn)
          ]
      )

  fromCql (C.CqlUdt fs) = do
    t <- required "typ"
    k <- required "key"
    s <- notrequired "size"
    case (t :: Int32) of
      0 -> pure $! ImageAsset k s
      _ -> Left $ "unexpected user asset type: " ++ show t
    where
      required :: (C.Cql r) => Text -> Either String r
      required f =
        maybe
          (Left ("Asset: Missing required field '" ++ show f ++ "'"))
          C.fromCql
          (lookup f fs)
      notrequired f = maybe (Right Nothing) C.fromCql (lookup f fs)
  fromCql _ = Left "UserAsset: UDT expected"

  -- Note: Order must match up with the 'ctype' definition.
  toCql (ImageAsset k s) =
    C.CqlUdt
      [ ("typ", C.CqlInt 0),
        ("key", C.toCql k),
        ("size", C.toCql s)
      ]

data AssetSize = AssetComplete | AssetPreview
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetSize)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AssetSize

instance ToSchema AssetSize where
  schema =
    enum @Text "AssetSize" $
      mconcat
        [ element "preview" AssetPreview,
          element "complete" AssetComplete
        ]

instance C.Cql AssetSize where
  ctype = C.Tagged C.IntColumn

  fromCql (C.CqlInt 0) = pure AssetPreview
  fromCql (C.CqlInt 1) = pure AssetComplete
  fromCql n = Left $ "Unexpected asset size: " ++ show n

  toCql AssetPreview = C.CqlInt 0
  toCql AssetComplete = C.CqlInt 1

--------------------------------------------------------------------------------
-- ManagedBy

-- TODO: This is now also used by UserGroups, the docs should reflect this.

-- | Who controls changes to the user profile (where the profile is defined as "all
-- user-editable, user-visible attributes").  See {#SparBrainDump}.
data ManagedBy
  = -- | The profile can be changed in-app; user doesn't show up via SCIM at all.
    ManagedByWire
  | -- | The profile can only be changed via SCIM, with several exceptions:
    --
    --   1. User properties can still be set (because they are used internally by clients
    --      and none of them can be modified via SCIM now or in the future).
    --
    --   2. Password can be changed by the user (SCIM doesn't support setting passwords yet,
    --      but currently SCIM only works with SSO-users who don't even have passwords).
    --
    --   3. The user can still be deleted normally (SCIM doesn't support deleting users yet;
    --      but it's questionable whether this should even count as a /change/ of a user
    --      profile).
    --
    -- There are some other things that SCIM can't do yet, like setting accent IDs, but they
    -- are not essential, unlike e.g. passwords.
    ManagedByScim
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ManagedBy)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ManagedBy)

instance ToSchema ManagedBy where
  schema =
    enum @Text "ManagedBy" $
      mconcat
        [ element "wire" ManagedByWire,
          element "scim" ManagedByScim
        ]

instance ToByteString ManagedBy where
  builder ManagedByWire = "wire"
  builder ManagedByScim = "scim"

instance FromByteString ManagedBy where
  parser =
    takeByteString >>= \case
      "wire" -> pure ManagedByWire
      "scim" -> pure ManagedByScim
      x -> fail $ "Invalid ManagedBy value: " <> show x

instance C.Cql ManagedBy where
  ctype = C.Tagged C.IntColumn

  fromCql (C.CqlInt n) = mapLeft Text.unpack $ managedByFromInt32 n
  fromCql n = Left $ "Unexpected ManagedBy: " ++ show n

  toCql = C.CqlInt . managedByToInt32

defaultManagedBy :: ManagedBy
defaultManagedBy = ManagedByWire

managedByToInt32 :: ManagedBy -> Int32
managedByToInt32 = \case
  ManagedByWire -> 0
  ManagedByScim -> 1

managedByFromInt32 :: Int32 -> Either Text ManagedBy
managedByFromInt32 = \case
  0 -> Right ManagedByWire
  1 -> Right ManagedByScim
  n -> Left $ "Unexpected ManagedBy: " <> Text.pack (show n)

--------------------------------------------------------------------------------
-- Deprecated

-- | DEPRECATED
newtype Pict = Pict {fromPict :: [A.Object]}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Pict

instance ToSchema Pict where
  schema =
    named "Pict" $
      Pict <$> fromPict .= untypedRangedSchema 0 10 (array jsonObject)

instance Arbitrary Pict where
  arbitrary = pure $ Pict []

instance C.Cql Pict where
  ctype = C.Tagged (C.ListColumn C.BlobColumn)

  fromCql (C.CqlList l) = do
    vs <- map (\(C.Blob lbs) -> lbs) <$> mapM C.fromCql l
    as <- mapM (note "Failed to read asset" . A.decode) vs
    pure $ Pict as
  fromCql _ = pure noPict

  toCql = C.toCql . map (C.Blob . A.encode) . fromPict

noPict :: Pict
noPict = Pict []
