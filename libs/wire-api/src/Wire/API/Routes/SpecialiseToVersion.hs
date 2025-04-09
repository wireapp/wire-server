-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

-- | Version-aware swagger generation
module Wire.API.Routes.SpecialiseToVersion where

import Data.Singletons.Base.TH
import GHC.TypeLits
import Servant
import Servant.API.Extended
import Servant.Multipart.API
import Wire.API.Deprecated
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.VersionInfo

type family SpecialiseToVersion (v :: k) api

type instance
  SpecialiseToVersion v (From w :> api) =
    If (v < w) EmptyAPI (SpecialiseToVersion v api)

type instance
  SpecialiseToVersion v (Until w :> api) =
    If (v < w) (SpecialiseToVersion v api) EmptyAPI

type instance
  SpecialiseToVersion v ((s :: Symbol) :> api) =
    s :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Named n api) =
    Named n (SpecialiseToVersion v api)

type instance
  SpecialiseToVersion v (NoContentVerb m) =
    NoContentVerb m

type instance
  SpecialiseToVersion v (Capture' mod sym a :> api) =
    Capture' mod sym a :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Summary s :> api) =
    Summary s :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Deprecated :> api) =
    Deprecated :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Verb m s t r) =
    Verb m s t r

type instance
  SpecialiseToVersion v (MultiVerb m t r x) =
    MultiVerb m t r x

type instance SpecialiseToVersion v RawM = RawM

type instance
  SpecialiseToVersion v (ReqBody t x :> api) =
    ReqBody t x :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (QueryParam' mods l x :> api) =
    QueryParam' mods l x :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Header' opts l x :> api) =
    Header' opts l x :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (Description desc :> api) =
    Description desc :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (StreamBody' opts f t x :> api) =
    StreamBody' opts f t x :> SpecialiseToVersion v api

type instance SpecialiseToVersion v EmptyAPI = EmptyAPI

type instance
  SpecialiseToVersion v (api1 :<|> api2) =
    SpecialiseToVersion v api1 :<|> SpecialiseToVersion v api2

type instance
  SpecialiseToVersion v (ReqBodyCustomError t l x :> api) =
    ReqBodyCustomError t l x :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (MultipartForm x b :> api) =
    MultipartForm x b :> SpecialiseToVersion v api

type instance
  SpecialiseToVersion v (CaptureAll sym tipe :> api) =
    CaptureAll sym tipe :> SpecialiseToVersion v api
