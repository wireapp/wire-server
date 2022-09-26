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

module Wire.API.Routes.Versioned where

import Data.Proxy
import Imports
import Servant.API

-- | A newtype wrapper for creating version-specific variants of types.
newtype Versioned v a = Versioned {unVersioned :: a}

instance Functor (Versioned v) where
  fmap f (Versioned a) = Versioned (f a)

instance Accept a => Accept (Versioned v a) where
  contentType _ = contentType (Proxy @a)
  contentTypes _ = contentTypes (Proxy @a)

-- | Turn an instance for a versioned value into an instance for the
-- unversioned value but versioned content type. This makes it possible to add
-- versioning to parts of an API without having the version leak into the
-- handler.
instance MimeUnrender ct (Versioned v a) => MimeUnrender (Versioned v ct) a where
  mimeUnrender _ = fmap (unVersioned @_ @v) . mimeUnrender (Proxy @ct)

-- | See corresponding 'MimeUnrender' instance for details.
instance MimeRender ct (Versioned v a) => MimeRender (Versioned v ct) a where
  mimeRender _ = mimeRender (Proxy @ct) . Versioned @v
