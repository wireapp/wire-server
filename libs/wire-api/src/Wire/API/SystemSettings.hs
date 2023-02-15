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

module Wire.API.SystemSettings where

import Control.Lens hiding ((.=))
import qualified Data.Aeson as A
import Data.Schema as Schema
import qualified Data.Swagger as S
import Imports
import Servant.Swagger.Internal.Orphans ()
import Test.QuickCheck
import Wire.Arbitrary

-- | Subset of `Brig.Options.Settings` that is safe to be shown in public.
--
-- Used to expose settings via the @/system/settings/unauthorized@ endpoint.
-- ALWAYS CHECK WITH SECURITY IF YOU WANT TO ADD SETTINGS HERE.
data SystemSettingsPublic = SystemSettingsPublic
  { sspSetRestrictUserCreation :: !Bool
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema SystemSettingsPublic
  deriving (Arbitrary) via (GenericUniform SystemSettingsPublic)

instance ToSchema SystemSettingsPublic where
  schema =
    object "SystemSettingsPublic" $ settingsPublicObjectSchema

settingsPublicObjectSchema :: ObjectSchema SwaggerDoc SystemSettingsPublic
settingsPublicObjectSchema =
  SystemSettingsPublic
    <$> sspSetRestrictUserCreation .= fieldWithDocModifier "setRestrictUserCreation" (description ?~ "Do not allow certain user creation flows") schema

data SystemSettingsInternal = SystemSettingsInternal
  { ssiSetEnableMls :: !Bool
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema SystemSettingsInternal
  deriving (Arbitrary) via (GenericUniform SystemSettingsInternal)

instance ToSchema SystemSettingsInternal where
  schema =
    object "SystemSettingsInternal" $ settingsInternalObjectSchema

settingsInternalObjectSchema :: ObjectSchema SwaggerDoc SystemSettingsInternal
settingsInternalObjectSchema =
  SystemSettingsInternal
    <$> ssiSetEnableMls .= fieldWithDocModifier "setEnableMls" (description ?~ "Whether MLS is enabled or not") schema

data SystemSettings = SystemSettings
  { ssPublic :: !SystemSettingsPublic,
    ssInternal :: !SystemSettingsInternal
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema SystemSettings
  deriving (Arbitrary) via (GenericUniform SystemSettings)

instance ToSchema SystemSettings where
  schema =
    object "SystemSettings" $
      SystemSettings
        <$> ssPublic .= settingsPublicObjectSchema
        <*> ssInternal .= settingsInternalObjectSchema
