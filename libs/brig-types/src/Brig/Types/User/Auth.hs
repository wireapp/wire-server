{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
  ( PendingLoginCode (..),
    LoginCode (..),
    SendLoginCode (..),
    LoginCodeTimeout (..),
    Login (..),
    SsoLogin (..),
    LegalHoldLogin (..),
    loginLabel,
    AccessToken (..),
    TokenType (..),
    bearerToken,
    RemoveCookies (..),
    CookieLabel (..),
    CookieId (..),
    Cookie (..),
    CookieList (..),
    CookieType (..),
  )
where

import Data.Id (UserId)
import Data.Misc (PlainTextPassword (..))
import Imports
import Wire.API.User.Auth

-- | A special kind of login that is only used for an internal endpoint.
data SsoLogin
  = SsoLogin !UserId !(Maybe CookieLabel)

-- | A special kind of login that is only used for an internal endpoint.
-- This kind of login returns restricted 'LegalHoldUserToken's instead of regular
-- tokens.
data LegalHoldLogin
  = LegalHoldLogin !UserId !(Maybe PlainTextPassword) !(Maybe CookieLabel)
