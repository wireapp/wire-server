{-# LANGUAGE OverloadedStrings #-}

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

module Brig.Types.User.Auth
  ( SsoLogin (..),
    LegalHoldLogin (..),
  )
where

import Data.Aeson
import Data.Id (UserId)
import Data.Misc (PlainTextPassword6)
import Imports
import Wire.API.User.Auth

-- | A special kind of login that is only used for an internal endpoint.
data SsoLogin
  = SsoLogin !UserId !(Maybe CookieLabel)

-- | A special kind of login that is only used for an internal endpoint.
-- This kind of login returns restricted 'LegalHoldUserToken's instead of regular
-- tokens.
data LegalHoldLogin
  = LegalHoldLogin !UserId !(Maybe PlainTextPassword6) !(Maybe CookieLabel)

instance FromJSON SsoLogin where
  parseJSON = withObject "SsoLogin" $ \o ->
    SsoLogin <$> o .: "user" <*> o .:? "label"

instance ToJSON SsoLogin where
  toJSON (SsoLogin uid label) =
    object ["user" .= uid, "label" .= label]

instance FromJSON LegalHoldLogin where
  parseJSON = withObject "LegalHoldLogin" $ \o ->
    LegalHoldLogin
      <$> o
        .: "user"
      <*> o
        .:? "password"
      <*> o
        .:? "label"

instance ToJSON LegalHoldLogin where
  toJSON (LegalHoldLogin uid password label) =
    object
      [ "user" .= uid,
        "password" .= password,
        "label" .= label
      ]
