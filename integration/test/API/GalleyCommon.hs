{-# LANGUAGE OverloadedStrings #-}

module API.GalleyCommon where

import Control.Lens ((?~))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Kind
import Data.OpenApi
import qualified Data.Schema as Schema
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Testlib.JSON
import Testlib.Prelude

data FeatureStatus = Disabled | Enabled
  deriving (Eq, Generic)
  deriving (ToJSON) via (Schema.Schema FeatureStatus)

instance Schema.ToSchema FeatureStatus where
  schema =
    Schema.enum @T.Text "FeatureStatus" $
      mconcat
        [ Schema.element "enabled" Enabled,
          Schema.element "disabled" Disabled
        ]

instance Show FeatureStatus where
  show = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

oppositeStatus :: FeatureStatus -> FeatureStatus
oppositeStatus Disabled = Enabled
oppositeStatus Enabled = Disabled

data LockStatus = LockStatusLocked | LockStatusUnlocked
  deriving stock (Eq)
  deriving (ToJSON, FromJSON) via (Schema.Schema LockStatus)

instance Show LockStatus where
  show = \case
    LockStatusLocked -> "locked"
    LockStatusUnlocked -> "unlocked"

instance Schema.ToSchema LockStatus where
  schema =
    Schema.enum @T.Text "LockStatus" $
      mconcat
        [ Schema.element "locked" LockStatusLocked,
          Schema.element "unlocked" LockStatusUnlocked
        ]

-- Using Word to avoid dealing with negative numbers.
-- Ideally we would also not support zero.
-- Currently a TTL=0 is ignored on the cassandra side.
data FeatureTTL
  = -- | actually, unit depends on phantom type.
    FeatureTTLSeconds Word
  | FeatureTTLUnlimited
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON) via (Schema.Schema FeatureTTL)

instance Schema.ToSchema FeatureTTL where
  schema = Schema.mkSchema ttlDoc toTTL fromTTL
    where
      ttlDoc :: Schema.NamedSwaggerDoc
      ttlDoc = Schema.swaggerDoc @Word & schema . example ?~ (Aeson.String . T.pack $ "unlimited")

      toTTL :: Aeson.Value -> Aeson.Parser FeatureTTL
      toTTL v = parseUnlimited v <|> parseSeconds v

      parseUnlimited :: Aeson.Value -> Aeson.Parser FeatureTTL
      parseUnlimited =
        Aeson.withText "FeatureTTL" $
          \t ->
            if t == "unlimited" || t == "0"
              then pure FeatureTTLUnlimited
              else Aeson.parseFail "Expected ''unlimited' or '0'."

      parseSeconds :: Aeson.Value -> Aeson.Parser FeatureTTL
      parseSeconds = Aeson.withScientific "FeatureTTL" $
        \s -> case toBoundedInteger s of
          Just 0 -> error "impossible (this would have parsed in `parseUnlimited` above)."
          Just i -> pure . FeatureTTLSeconds $ i
          Nothing -> Aeson.parseFail "Expected an positive integer."

      fromTTL :: FeatureTTL -> Maybe Aeson.Value
      fromTTL FeatureTTLUnlimited = Just . Aeson.String . T.pack $ "unlimited"
      fromTTL (FeatureTTLSeconds 0) = Nothing -- Should be unlimited
      fromTTL (FeatureTTLSeconds s) = Just $ Aeson.toJSON s

data WithStatusNoLock (cfg :: Type) = WithStatusNoLock
  { status :: FeatureStatus,
    config :: cfg,
    ttl :: FeatureTTL
  }

instance ToJSON cfg => MakesValue (WithStatusNoLock cfg) where
  make = pure . toJSON

instance ToJSON cfg => ToJSON (WithStatusNoLock cfg) where
  toJSON (WithStatusNoLock s cfg t) =
    Aeson.object $
      [ "status" .= s,
        "ttl" .= toJSON t
      ] -- NOTE(md): The "config" part should probably be absent in case of a trivial config
        <> ["config" .= toJSON cfg]

data WithStatus (cfg :: Type) = WithStatus
  { wsStatus :: FeatureStatus,
    wsLockStatus :: LockStatus,
    wsConfig :: cfg,
    wsTTL :: FeatureTTL
  }

deriving instance (Eq cfg) => Eq (WithStatus cfg)

deriving instance (Show cfg) => Show (WithStatus cfg)

deriving via (Schema.Schema (WithStatus cfg)) instance (Schema.ToSchema (WithStatus cfg)) => ToJSON (WithStatus cfg)

instance (Schema.ToSchema cfg, IsFeatureConfig cfg) => Schema.ToSchema (WithStatus cfg) where
  schema =
    Schema.object sn $
      WithStatus
        <$> wsStatus
        Schema..= Schema.field "status" Schema.schema
        <*> wsLockStatus
        Schema..= Schema.field "lockStatus" Schema.schema
        <*> wsConfig
        Schema..= objectSchema @cfg
        <*> wsTTL
        Schema..= (fromMaybe FeatureTTLUnlimited <$> Schema.optField "ttl" Schema.schema)
    where
      inner = Schema.schema @cfg
      sn = fromMaybe "" (Schema.getName (Schema.schemaDoc inner)) <> ".WithStatus"

--------------------------------------------------------------------------------
-- Feature configurations

class IsFeatureConfig cfg where
  objectSchema ::
    -- | Should be "pure MyFeatureConfig" if the feature doesn't have config,
    -- which results in a trivial empty schema and the "config" field being
    -- omitted/ignored in the JSON encoder / parser.
    Schema.ObjectSchema Schema.SwaggerDoc cfg

data TrivialConfig = TrivialConfig
  deriving (ToJSON) via (Schema.Schema TrivialConfig)

instance Schema.ToSchema TrivialConfig where
  schema = Schema.object "TrivialConfig" objectSchema

instance IsFeatureConfig TrivialConfig where
  objectSchema = pure TrivialConfig

data ClassifiedDomainsConfig = ClassifiedDomainsConfig
  { classifiedDomainsDomains :: [T.Text]
  }
  deriving stock (Show, Eq)

instance ToJSON ClassifiedDomainsConfig where
  toJSON (ClassifiedDomainsConfig ds) =
    Aeson.object $
      ["domains" .= Aeson.Array (Vector.fromList (Aeson.String <$> ds))]

newtype SelfDeletingMessagesConfig = SelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Schema.Schema SelfDeletingMessagesConfig)

instance Schema.ToSchema SelfDeletingMessagesConfig where
  schema =
    Schema.object "SelfDeletingMessagesConfig" $
      SelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds
        Schema..= Schema.field "enforcedTimeoutSeconds" Schema.schema

instance IsFeatureConfig SelfDeletingMessagesConfig where
  objectSchema = Schema.field "config" Schema.schema

data GuestLinksConfig = GuestLinksConfig
  deriving stock (Show, Eq)
  deriving (ToJSON) via (Schema.Schema GuestLinksConfig)

instance IsFeatureConfig GuestLinksConfig where
  objectSchema = pure GuestLinksConfig

instance Schema.ToSchema GuestLinksConfig where
  schema = Schema.object "GuestLinksConfig" objectSchema