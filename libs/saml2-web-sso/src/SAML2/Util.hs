{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module SAML2.Util
  ( module SAML2.Util,
    module Text.XML.Util,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.String.Conversions
import Data.Text qualified as ST
import Data.Typeable
import GHC.Stack
import Text.XML.Util
import URI.ByteString

renderURI :: URI -> ST
renderURI = cs . serializeURIRef'

parseURI' :: (MonadError String m) => ST -> m URI
parseURI' uri = either (die' (Just $ show uri) (Proxy @URI)) pure . parseURI laxURIParserOptions . cs . ST.strip $ uri

-- | You probably should not use this.  If you have a string literal, consider "URI.ByteString.QQ".
unsafeParseURI :: ST -> URI
unsafeParseURI = either (error . ("could not parse config: " <>) . show) id . parseURI'

-- | @uriSegments "/one/two" == uriSegments "one/two/" == uriSegments "///one//two///" == ["one", "two"]@
uriSegments :: ST -> [ST]
uriSegments = filter (not . ST.null) . ST.splitOn "/"

uriUnSegments :: [ST] -> ST
uriUnSegments = ("/" <>) . ST.intercalate "/"

(-/) :: (HasCallStack) => ST -> ST -> ST
oldpath -/ pathext = uriUnSegments . uriSegments $ oldpath <> "/" <> pathext

(=/) :: (HasCallStack) => URI -> ST -> URI
uri =/ pathext = normURI $ uri & pathL %~ (<> "/" <> cs pathext)

normURI :: URI -> URI
normURI =
  unsafeParseURI
    . cs
    . normalizeURIRef'
      URINormalizationOptions
        { unoDowncaseScheme = True,
          unoDowncaseHost = True,
          unoDropDefPort = False,
          unoSlashEmptyPath = True,
          unoDropExtraSlashes = True,
          unoSortParameters = True,
          unoRemoveDotSegments = True,
          unoDefaultPorts = mempty
        }
