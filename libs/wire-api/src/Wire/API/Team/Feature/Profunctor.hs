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

-- | * schema-profunctor utilities for team features.
--
-- This module contains utilities for building schemas for team features in
-- "Barbie" style, i.e. parameterised on a functor @f@. The functor can
-- normally be instantiated to either 'Identity' (for API values) or 'Maybe'
-- (for database values).
module Wire.API.Team.Feature.Profunctor where

import Data.Default
import Data.Profunctor
import Data.Schema
import Imports

-- | Parse an optional field by using its default when @f@ is 'Identity', and
-- leaving it as 'Nothing' when @f@ is 'Maybe'.
class OptWithDefault f where
  fromOpt :: (Default a) => ObjectSchemaP SwaggerDoc a (Maybe a) -> ObjectSchema SwaggerDoc (f a)

instance OptWithDefault Maybe where
  fromOpt = maybe_

instance OptWithDefault Identity where
  fromOpt = dimap runIdentity Identity . fmap (fromMaybe def)

-- | This class enables non-standard JSON instances for the 'Identity' case of
-- this feature. In some cases, for backwards compatibility, we need to make a
-- field optional even in the 'Identity' case. A missing field gets parsed as
-- 'Nothing'. Whereas with the default instance, they would be rejected.
class NestedMaybe f where
  nestedMaybeField :: Text -> ValueSchema SwaggerDoc a -> ObjectSchema SwaggerDoc (f (Maybe a))

instance NestedMaybe Maybe where
  nestedMaybeField name sch = maybe_ (optField name (nullable sch))

instance NestedMaybe Identity where
  nestedMaybeField name sch = Identity <$> runIdentity .= maybe_ (optField name sch)
