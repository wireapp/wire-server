module API.GalleyCommon where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Testlib.JSON
import Testlib.Prelude

data WithStatusNoLock = WithStatusNoLock
  { status :: FeatureStatus,
    ttl :: Word
  }

instance MakesValue WithStatusNoLock where
  make = pure . toJSON

instance ToJSON WithStatusNoLock where
  toJSON (WithStatusNoLock s t) =
    Aeson.object
      [ "status" .= s,
        "ttl" .= case t of
          0 -> "unlimited"
          n -> show n
      ]

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
