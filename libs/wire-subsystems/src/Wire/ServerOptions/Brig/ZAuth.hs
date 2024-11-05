{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ServerOptions.Brig.ZAuth
  ( Settings (..),
    keyIndex,
    userTokenTimeout,
    sessionTokenTimeout,
    accessTokenTimeout,
    providerTokenTimeout,
    legalHoldUserTokenTimeout,
    legalHoldAccessTokenTimeout,
    defSettings,
    UserTokenTimeout (..),
    userTokenTimeoutSeconds,
    SessionTokenTimeout (..),
    AccessTokenTimeout (..),
    accessTokenTimeoutSeconds,
    ProviderTokenTimeout (..),
    LegalHoldUserTokenTimeout (..),
    legalHoldUserTokenTimeoutSeconds,
    LegalHoldAccessTokenTimeout (..),
    legalHoldAccessTokenTimeoutSeconds,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Imports

data Settings = Settings
  { -- | Secret key index to use
    --   for token creation
    _keyIndex :: !Int,
    -- | User token validity timeout
    _userTokenTimeout :: !UserTokenTimeout,
    -- | Session token validity timeout
    _sessionTokenTimeout :: !SessionTokenTimeout,
    -- | Access token validity timeout
    _accessTokenTimeout :: !AccessTokenTimeout,
    -- | Proider token validity timeout
    _providerTokenTimeout :: !ProviderTokenTimeout,
    -- | Legal Hold User token validity timeout
    _legalHoldUserTokenTimeout :: !LegalHoldUserTokenTimeout,
    -- | Legal Hold Access token validity timeout
    _legalHoldAccessTokenTimeout :: !LegalHoldAccessTokenTimeout
  }
  deriving (Show, Generic)

defSettings :: Settings
defSettings =
  Settings
    1
    (UserTokenTimeout (60 * 60 * 24 * 28)) -- 28 days
    (SessionTokenTimeout (60 * 60 * 24)) -- 1 day
    (AccessTokenTimeout 900) -- 15 minutes
    (ProviderTokenTimeout (60 * 60 * 24 * 7)) -- 7 days
    (LegalHoldUserTokenTimeout (60 * 60 * 24 * 56)) -- 56 days
    (LegalHoldAccessTokenTimeout (60 * 15)) -- 15 minutes

newtype UserTokenTimeout = UserTokenTimeout
  {_userTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype SessionTokenTimeout = SessionTokenTimeout
  {sessionTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype AccessTokenTimeout = AccessTokenTimeout
  {_accessTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype ProviderTokenTimeout = ProviderTokenTimeout
  {providerTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldUserTokenTimeout = LegalHoldUserTokenTimeout
  {_legalHoldUserTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldAccessTokenTimeout = LegalHoldAccessTokenTimeout
  {_legalHoldAccessTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

instance FromJSON UserTokenTimeout

instance FromJSON SessionTokenTimeout

instance FromJSON AccessTokenTimeout

instance FromJSON ProviderTokenTimeout

instance FromJSON LegalHoldAccessTokenTimeout

instance FromJSON LegalHoldUserTokenTimeout

instance FromJSON Settings where
  parseJSON = withObject "ZAuth.Settings" $ \o ->
    Settings
      <$> o .: "keyIndex"
      <*> (UserTokenTimeout <$> o .: "userTokenTimeout")
      <*> (SessionTokenTimeout <$> o .: "sessionTokenTimeout")
      <*> (AccessTokenTimeout <$> o .: "accessTokenTimeout")
      <*> (ProviderTokenTimeout <$> o .: "providerTokenTimeout")
      <*> (LegalHoldUserTokenTimeout <$> o .: "legalHoldUserTokenTimeout")
      <*> (LegalHoldAccessTokenTimeout <$> o .: "legalHoldAccessTokenTimeout")

makeLenses ''LegalHoldAccessTokenTimeout
makeLenses ''AccessTokenTimeout

makeLenses ''UserTokenTimeout

makeLenses ''LegalHoldUserTokenTimeout

makeLenses ''Settings
