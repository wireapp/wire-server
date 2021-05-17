{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Cookie where

import Control.Monad.Except
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions
import Data.Swagger
import Imports
import SAML2.WebSSO (SimpleSetCookie)
import qualified SAML2.WebSSO as SAML
import Web.Cookie

type SetBindCookie = SimpleSetCookie "zbind"

newtype BindCookie = BindCookie {fromBindCookie :: ST}

instance ToParamSchema SetBindCookie where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToParamSchema BindCookie where
  toParamSchema _ = toParamSchema (Proxy @String)

-- | Extract @zbind@ cookie from HTTP header contents if it exists.
bindCookieFromHeader :: ST -> Maybe BindCookie
bindCookieFromHeader = fmap BindCookie . lookup "zbind" . parseCookiesText . cs

-- (we could rewrite this as @SAML.cookieName SetBindCookie@ if 'cookieName'
-- accepted any @proxy :: Symbol -> *@ rather than just 'Proxy'.)

setBindCookieValue :: HasCallStack => SetBindCookie -> BindCookie
setBindCookieValue = BindCookie . cs . setCookieValue . SAML.fromSimpleSetCookie
