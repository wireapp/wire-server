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

module Wire.API.Routes.Cookies where

import Control.Error.Util
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Metrics.Servant
import Data.SOP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.TypeLits
import Imports
import Servant
import Servant.Swagger
import Web.Cookie (parseCookies)

data (:::) a b

data Cookies (cs :: [*])

type CookieHeader cs = Header' '[Required] "Cookie" (CookieTuple cs)

type CookieType = NonEmpty

type family CookieTypes (cs :: [*]) :: [*]

type instance CookieTypes '[] = '[]

type instance CookieTypes ((lbl ::: x) ': cs) = (CookieType x ': CookieTypes cs)

newtype CookieTuple cs = CookieTuple {unCookieTuple :: NP I (CookieTypes cs)}

type CookieMap = Map ByteString (NonEmpty ByteString)

instance HasSwagger api => HasSwagger (Cookies cs :> api) where
  -- TODO
  toSwagger _ = toSwagger (Proxy @api)

class CookieArgs (cs :: [*]) where
  type AddArgs cs a :: *

  addArgs :: AddArgs cs a -> CookieTuple cs -> a
  mapArgs :: (a -> b) -> AddArgs cs a -> AddArgs cs b
  mkTuple :: CookieMap -> Either Text (CookieTuple cs)

instance CookieArgs '[] where
  type AddArgs '[] a = a
  addArgs a _ = a
  mapArgs h = h
  mkTuple _ = pure (CookieTuple Nil)

instance
  ( CookieArgs cs,
    KnownSymbol lbl,
    FromHttpApiData x
  ) =>
  CookieArgs ((lbl ::: (x :: *)) ': cs)
  where
  type AddArgs ((lbl ::: x) ': cs) a = CookieType x -> AddArgs cs a
  addArgs f (CookieTuple (I x :* xs)) = addArgs @cs (f x) (CookieTuple xs)
  mapArgs h f = mapArgs @cs h . f
  mkTuple m = do
    let k = T.pack (symbolVal (Proxy @lbl))
    bs <- note ("Missing cookie: " <> k) $ M.lookup (T.encodeUtf8 k) m
    vs <- traverse parseHeader bs
    CookieTuple t <- mkTuple @cs m
    pure (CookieTuple (I vs :* t))

mkCookieMap :: [(ByteString, ByteString)] -> CookieMap
mkCookieMap = foldr (\(k, v) -> M.insertWith (<>) k (pure v)) mempty

instance CookieArgs cs => FromHttpApiData (CookieTuple cs) where
  parseHeader = mkTuple . mkCookieMap . parseCookies
  parseUrlPiece = parseHeader . T.encodeUtf8

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    CookieArgs cs,
    HasServer api ctx
  ) =>
  HasServer (Cookies cs :> api) ctx
  where
  type ServerT (Cookies cs :> api) m = AddArgs cs (ServerT api m)

  route _ ctx action =
    route (Proxy @(CookieHeader cs :> api)) ctx (fmap addArgs action)
  hoistServerWithContext _ ctx f = mapArgs @cs (hoistServerWithContext (Proxy @api) ctx f)

instance RoutesToPaths api => RoutesToPaths (Cookies cs :> api) where
  getRoutes = getRoutes @api
