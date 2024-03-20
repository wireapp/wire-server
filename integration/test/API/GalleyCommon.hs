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

instance Show FeatureStatus where
  show = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

instance ToJSON FeatureStatus where
  toJSON = Aeson.String . T.pack . show

oppositeStatus :: FeatureStatus -> FeatureStatus
oppositeStatus Disabled = Enabled
oppositeStatus Enabled = Disabled

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
            if t == (T.pack "unlimited") || t == (T.pack "0")
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


--------------------------------------------------------------------------------
-- Feature configurations

data ClassifiedDomainsConfig = ClassifiedDomainsConfig
  { classifiedDomainsDomains :: [T.Text]
  }
  deriving stock (Show, Eq)

instance ToJSON ClassifiedDomainsConfig where
  toJSON (ClassifiedDomainsConfig ds) =
    Aeson.object $
      ["domains" .= Aeson.Array (Vector.fromList (Aeson.String <$> ds))]

data LockStatus = LockStatusLocked | LockStatusUnlocked
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via (Schema.Schema LockStatus)

instance Schema.ToSchema LockStatus where
  schema =
    Schema.enum @T.Text (T.pack "LockStatus") $
      mconcat
        [ Schema.element (T.pack "locked") LockStatusLocked,
          Schema.element (T.pack "unlocked") LockStatusUnlocked
        ]
