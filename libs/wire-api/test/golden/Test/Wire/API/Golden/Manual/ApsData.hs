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

module Test.Wire.API.Golden.Manual.ApsData
  ( testObject_ApsData_1,
    testObject_ApsData_2,
  )
where

import Data.Aeson
import Data.Json.Util
import Imports

newtype ApsSound = ApsSound {fromSound :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ApsLocKey = ApsLocKey {fromLocKey :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

data ApsData = ApsData
  { _apsLocKey :: !ApsLocKey,
    _apsLocArgs :: [Text],
    _apsSound :: !(Maybe ApsSound),
    _apsBadge :: !Bool
  }
  deriving (Eq, Show)

instance ToJSON ApsData where
  toJSON (ApsData k a s b) =
    object $
      "loc_key"
        .= k
        # "loc_args"
        .= a
        # "sound"
        .= s
        # "badge"
        .= b
        # []

instance FromJSON ApsData where
  parseJSON = withObject "ApsData" $ \o ->
    ApsData
      <$> o .: "loc_key"
      <*> o .:? "loc_args" .!= []
      <*> o .:? "sound"
      <*> o .:? "badge" .!= True

apsData :: ApsLocKey -> [Text] -> ApsData
apsData lk la = ApsData lk la Nothing True

testObject_ApsData_1 :: ApsData
testObject_ApsData_1 =
  apsData (ApsLocKey mempty) mempty

testObject_ApsData_2 :: ApsData
testObject_ApsData_2 =
  apsData (ApsLocKey "asdf") ["1", "22", "333"]
