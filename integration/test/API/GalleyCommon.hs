module API.GalleyCommon where

import qualified Data.Aeson as Aeson
import Data.Kind
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Testlib.JSON
import Testlib.Prelude

data WithStatusNoLock (cfg :: Type) = WithStatusNoLock
  { status :: FeatureStatus,
    config :: cfg,
    ttl :: Word
  }

instance ToJSON cfg => MakesValue (WithStatusNoLock cfg) where
  make = pure . toJSON

instance ToJSON cfg => ToJSON (WithStatusNoLock cfg) where
  toJSON (WithStatusNoLock s cfg t) =
    Aeson.object $
      [ "status" .= s,
        "ttl" .= case t of
          0 -> "unlimited"
          n -> show n
      ] -- NOTE(md): The "config" part should probably be absent in case of a trivial config
        <> ["config" .= toJSON cfg]

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
