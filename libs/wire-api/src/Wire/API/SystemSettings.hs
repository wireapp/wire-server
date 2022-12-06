module Wire.API.SystemSettings where

import Control.Lens hiding ((.=))
import qualified Data.Aeson as A
import Data.Schema as Schema
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant.Swagger.Internal.Orphans ()
import Test.QuickCheck
import Wire.Arbitrary

-- | Subset of `Brig.Options.Settings` that is safe to be shown in public.
--
-- Used to expose settings via the @/system/settings@ endpoint. Please check
-- with security if you want to add settings here.
data SystemSettings = SystemSettings
  { systemSettingsSetRestrictUserCreation :: !Bool
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema.Schema SystemSettings
  deriving (Arbitrary) via (GenericUniform SystemSettings)

instance Schema.ToSchema SystemSettings where
  schema =
    Schema.object "SystemSettings" $
      SystemSettings
        <$> systemSettingsSetRestrictUserCreation
          Schema..= Schema.fieldWithDocModifier
            "setRestrictUserCreation"
            (description ?~ "Do not allow certain user creation flows")
            Schema.schema
