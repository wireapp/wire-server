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

module Wire.API.Routes.MultiVerb where

import Data.Proxy
import Data.SOP
import Data.SOP.NS
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (HeaderName, hContentType)
import Network.HTTP.Types.Status
import Network.Wai (Request, Response, ResponseReceived, responseLBS)
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Status (KnownStatus (..))
import Servant.Server
import Servant.Server.Internal
import Servant.Swagger as S
import UnliftIO.Resource (runResourceT)

type Declare = S.Declare (S.Definitions S.Schema)

data Respond (cs :: [*]) (s :: Nat) (desc :: Symbol) (a :: *)

data ResponseSwagger = ResponseSwagger
  { rsDescription :: Text,
    rsSchema :: S.NamedSchema
  }

data RenderOutput = RenderOutput
  { roStatus :: Status,
    roBody :: LByteString,
    roHeaders :: [(HeaderName, ByteString)]
  }

class Render (cs :: [*]) x where
  render :: x -> [LByteString]

instance (MimeRender c x, Render cs x) => Render (c ': cs) x where
  render x = mimeRender (Proxy @c) x : render @cs x

instance Render '[] () where
  render () = mempty

class IsResponse a where
  type ResponseType a :: *
  type ResponseContentTypes a :: [*]

  responseRender :: ResponseType a -> [RenderOutput]
  responseSwagger :: Declare ResponseSwagger

instance
  ( Render cs a,
    KnownStatus s,
    KnownSymbol desc,
    S.ToSchema a
  ) =>
  IsResponse (Respond cs s desc a)
  where
  type ResponseType (Respond cs s desc a) = a
  type ResponseContentTypes (Respond cs s desc a) = cs

  responseSwagger = ResponseSwagger desc <$> S.declareNamedSchema (Proxy @a)
    where
      desc = Text.pack (symbolVal (Proxy @desc))
  responseRender x = map mkRenderOutput (render @cs x)
    where
      mkRenderOutput :: LByteString -> RenderOutput
      mkRenderOutput body =
        RenderOutput
          { roStatus = statusVal (Proxy @s),
            roBody = body,
            roHeaders = [] -- TODO
          }

class IsResponseList as where
  type ResponseTypes as :: [*]

  responseListContentTypes :: [[M.MediaType]]
  responseListRender :: Union (ResponseTypes as) -> [RenderOutput]

instance IsResponseList '[] where
  type ResponseTypes '[] = '[]

  responseListContentTypes = []

  responseListRender x = case x of

instance (AllMime (ResponseContentTypes a), IsResponse a, IsResponseList as) => IsResponseList (a ': as) where
  type ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

  responseListContentTypes = allMime (Proxy @(ResponseContentTypes a)) : responseListContentTypes @as

  responseListRender :: Union (ResponseType a ': ResponseTypes as) -> [RenderOutput]
  responseListRender (Z (I x)) = responseRender @a x
  responseListRender (S xs) = responseListRender @as xs

data MultiVerb (method :: StdMethod) (as :: [*]) (r :: *)

class IsResponseList as => AsUnion (as :: [*]) (r :: *) where
  asUnion :: r -> Union (ResponseTypes as)

instance (IsResponseList as, rs ~ ResponseTypes as) => AsUnion as (Union rs) where
  asUnion = id

instance
  ( KnownStatus s1,
    KnownStatus s2,
    KnownSymbol desc1,
    KnownSymbol desc2
  ) =>
  AsUnion '[Respond '[] s1 desc1 (), Respond '[] s2 desc2 ()] Bool
  where
  asUnion True = Z (I ())
  asUnion False = S (Z (I ()))

instance S.HasSwagger (MultiVerb method as r) where
  toSwagger _ = undefined -- TODO

roResponse :: RenderOutput -> Response
roResponse ro = responseLBS (roStatus ro) (roHeaders ro) (roBody ro)

roAddContentType :: M.MediaType -> RenderOutput -> RenderOutput
roAddContentType c ro = ro {roHeaders = (hContentType, M.renderHeader c) : roHeaders ro}

-- RouteResultT utilities

handlerToRouteResult :: MonadIO m => Handler a -> RouteResultT m a
handlerToRouteResult h = do
  ea <- liftIO (runHandler h)
  case ea of
    Left e -> RouteResultT (pure (FailFatal e))
    Right a -> pure a

hoistRouteResult :: (forall x. m x -> n x) -> RouteResultT m a -> RouteResultT n a
hoistRouteResult f (RouteResultT m) = RouteResultT (f m)

instance (AsUnion as r, ReflectMethod method) => HasServer (MultiVerb method as r) ctx where
  type ServerT (MultiVerb method as r) m = m r

  hoistServerWithContext _ _ nt s = nt s

  route ::
    forall env.
    Proxy (MultiVerb method as r) ->
    Context ctx ->
    Delayed env (Handler r) ->
    Router env
  route _ _ action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
      route' env req k = do
        let action' :: Delayed env (Handler r)
            action' = action `addMethodCheck` methodCheck method req
        (>>= k) . runRouteResultT . hoistRouteResult runResourceT $ do
          handler <- RouteResultT $ runDelayed action' env req
          (output :: r) <- handlerToRouteResult handler
          mkResponse req (asUnion @as output :: Union (ResponseTypes as))

      matchContentType :: [M.MediaType] -> AcceptHeader -> Maybe (Int, M.MediaType)
      matchContentType cs (AcceptHeader h) = do
        c <- M.matchAccept cs h
        i <- elemIndex c cs
        pure (i, c)

      mkResponse :: Monad m => Request -> Union (ResponseTypes as) -> RouteResultT m Response
      mkResponse req output = do
        let i = index_NS output
            cs = responseListContentTypes @as !! i
        resp <- case matchContentType cs (getAcceptHeader req) of
          Nothing -> RouteResultT . pure $ Fail err406
          Just (j, c) -> pure (roAddContentType c (responseListRender @as output !! j))
        let resp'
              | allowedMethodHead method req = resp {roBody = mempty}
              | otherwise = resp
        pure $ roResponse resp'
