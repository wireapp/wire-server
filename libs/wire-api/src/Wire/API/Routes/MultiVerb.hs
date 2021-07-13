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

module Wire.API.Routes.MultiVerb
  ( -- * MultiVerb types
    MultiVerb,
    Respond,
    WithHeaders,
    DescHeader,
    AsHeaders (..),
    AsUnion (..),
    IsResponse (..),
    IsResponseList (..),
  )
where

import Control.Lens hiding (Context)
import Data.Containers.ListUtils
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Proxy
import Data.SOP
import Data.SOP.NS
import qualified Data.Sequence as Seq
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (HeaderName, hContentType)
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Servant.API
import Servant.API.ContentTypes
import Servant.API.ResponseHeaders
import Servant.API.Status (KnownStatus (..))
import Servant.Client
import Servant.Client.Core
import Servant.Server
import Servant.Server.Internal
import Servant.Swagger as S
import Servant.Swagger.Internal as S
import UnliftIO.Resource (runResourceT)

type Declare = S.Declare (S.Definitions S.Schema)

-- | A type to describe a 'MultiVerb' response.
--
-- Includes a list of content types, status code, description, and a return type.
data Respond (cs :: [*]) (s :: Nat) (desc :: Symbol) (a :: *)

data ResponseSwagger = ResponseSwagger
  { rsDescription :: Text,
    rsStatus :: Status,
    rsHeaders :: InsOrdHashMap S.HeaderName S.Header,
    rsSchema :: Maybe (S.Referenced S.Schema)
  }

data RenderOutput = RenderOutput
  { roStatus :: Status,
    roBody :: LByteString,
    roHeaders :: [(HeaderName, ByteString)]
  }

class AllMime cs => Render (cs :: [*]) x where
  render :: x -> [LByteString]

instance MimeRender c x => Render '[c] x where
  render x = [mimeRender (Proxy @c) x]

instance (MimeRender c1 x, Render (c2 ': cs) x) => Render (c1 ': c2 ': cs) x where
  render x = mimeRender (Proxy @c1) x : render @(c2 ': cs) x

instance Render '[] () where
  render () = mempty

class IsResponse a where
  type ResponseType a :: *
  type ResponseContentTypes a :: [*]
  type ResponseStatus a :: Nat

  responseRender :: ResponseType a -> [RenderOutput]
  responseSwagger :: Declare ResponseSwagger

responseContentTypes ::
  forall a.
  AllMime (ResponseContentTypes a) =>
  [M.MediaType]
responseContentTypes = allMime (Proxy @(ResponseContentTypes a))

responseStatus ::
  forall a.
  KnownStatus (ResponseStatus a) =>
  Status
responseStatus = statusVal (Proxy @(ResponseStatus a))

class GenerateSchemaRef (cs :: [*]) a where
  generateSchemaRef :: Declare (Maybe (S.Referenced S.Schema))

instance GenerateSchemaRef '[] a where
  generateSchemaRef = pure Nothing

instance S.ToSchema a => GenerateSchemaRef (c ': cs) a where
  generateSchemaRef = Just <$> S.declareSchemaRef (Proxy @a)

instance
  ( Render cs a,
    KnownStatus s,
    KnownSymbol desc,
    S.ToSchema a,
    GenerateSchemaRef cs a
  ) =>
  IsResponse (Respond cs s desc a)
  where
  type ResponseType (Respond cs s desc a) = a
  type ResponseContentTypes (Respond cs s desc a) = cs
  type ResponseStatus (Respond cs s desc a) = s

  responseSwagger = ResponseSwagger desc status mempty <$> generateSchemaRef @cs @a
    where
      desc = Text.pack (symbolVal (Proxy @desc))
      status = statusVal (Proxy @s)

  responseRender x = map mkRenderOutput (render @cs x)
    where
      mkRenderOutput :: LByteString -> RenderOutput
      mkRenderOutput body =
        RenderOutput
          { roStatus = statusVal (Proxy @s),
            roBody = body,
            roHeaders = []
          }

-- | This type adds response headers to a 'MultiVerb' response.
--
-- Type variables:
--  * @hs@: type-level list of headers
--  * @a@: return type (with headers)
--  * @r@: underlying response (without headers)
data WithHeaders (hs :: [*]) (a :: *) (r :: *)

-- | This is used to convert a response containing headers to a custom type
-- including the information in the headers.
class AsHeaders hs a b where
  fromHeaders :: Headers hs a -> b
  toHeaders :: b -> Headers hs a

instance AsHeaders hs a (Headers hs a) where
  fromHeaders = id
  toHeaders = id

data DescHeader (name :: Symbol) (desc :: Symbol) (a :: *)

-- convert a list of Header to a list of Servant.Header
type family ServantHeaders (hs :: [*]) :: [*]

type instance ServantHeaders '[] = '[]

type instance
  ServantHeaders (DescHeader name desc a ': hs) =
    Header name a ': ServantHeaders hs

type instance
  ServantHeaders (Header name a ': hs) =
    Header name a ': ServantHeaders hs

instance
  (KnownSymbol name, KnownSymbol desc, S.ToParamSchema a) =>
  ToResponseHeader (DescHeader name desc a)
  where
  toResponseHeader _ = (name, S.Header (Just desc) sch)
    where
      name = Text.pack (symbolVal (Proxy @name))
      desc = Text.pack (symbolVal (Proxy @desc))
      sch = S.toParamSchema (Proxy @a)

instance
  ( AsHeaders (ServantHeaders hs) (ResponseType r) a,
    GetHeaders' (ServantHeaders hs),
    AllToResponseHeader hs,
    IsResponse r
  ) =>
  IsResponse (WithHeaders hs a r)
  where
  type ResponseType (WithHeaders hs a r) = a
  type ResponseContentTypes (WithHeaders hs a r) = ResponseContentTypes r
  type ResponseStatus (WithHeaders hs a r) = ResponseStatus r

  responseSwagger =
    fmap
      (\rs -> rs {rsHeaders = toAllResponseHeaders (Proxy @hs)})
      (responseSwagger @r)

  responseRender x = map addHeaders rs
    where
      h = toHeaders @(ServantHeaders hs) x
      rs = responseRender @r (getResponse h)
      addHeaders r = r {roHeaders = roHeaders r ++ getHeaders h}

class IsResponseList as where
  type ResponseTypes as :: [*]

  responseListContentTypes :: [[M.MediaType]]
  responseListRender :: Union (ResponseTypes as) -> [RenderOutput]
  responseListSwagger :: Declare [ResponseSwagger]
  responseListStatuses :: [Status]

instance IsResponseList '[] where
  type ResponseTypes '[] = '[]

  responseListContentTypes = []

  responseListRender x = case x of

  responseListSwagger = pure []

  responseListStatuses = []

instance
  ( AllMime (ResponseContentTypes a),
    IsResponse a,
    IsResponseList as,
    KnownStatus (ResponseStatus a)
  ) =>
  IsResponseList (a ': as)
  where
  type ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

  responseListContentTypes = responseContentTypes @a : responseListContentTypes @as

  responseListRender :: Union (ResponseType a ': ResponseTypes as) -> [RenderOutput]
  responseListRender (Z (I x)) = responseRender @a x
  responseListRender (S xs) = responseListRender @as xs

  responseListSwagger = (:) <$> responseSwagger @a <*> responseListSwagger @as

  responseListStatuses =
    responseStatus @a :
    responseListStatuses @as

-- | This type can be used in Servant to produce an endpoint which can return
-- multiple values with various content types and status codes. It is similar to
-- 'UVerb', but it has some important differences:
--
--  * Content types are established dynamically based on the return type of the
--    handler. In case of a content type mismatch with the Accept header, a 406
--    is returned and no other endpoint is tried.
--  * Descriptions and statuses can be attached to individual responses without
--    using wrapper types and without affecting the handler return type.
--  * The return type of the handler can be decoupled from the types of the
--    individual responses. One can use a 'Union' type just like for 'UVerb',
--    but 'MultiVerb' also supports using an arbitrary type with an 'AsUnion'
--    instance.
data MultiVerb (method :: StdMethod) (as :: [*]) (r :: *)

-- | This class is used to convert a handler return type to a union type
-- including all possible responses of a 'MultiVerb' endpoint.
class AsUnion (as :: [*]) (r :: *) where
  toUnion :: r -> Union (ResponseTypes as)
  fromUnion :: Union (ResponseTypes as) -> r

instance (IsResponseList as, rs ~ ResponseTypes as) => AsUnion as (Union rs) where
  toUnion = id
  fromUnion = id

instance
  AsUnion
    '[ Respond '[] s1 desc1 (),
       Respond '[] s2 desc2 ()
     ]
    Bool
  where
  toUnion False = Z (I ())
  toUnion True = S (Z (I ()))

  fromUnion (Z (I ())) = False
  fromUnion (S (Z (I ()))) = True
  fromUnion (S (S x)) = case x of

instance
  (ResponseType r2 ~ a) =>
  AsUnion
    '[Respond '[] s1 desc1 (), r2]
    (Maybe a)
  where
  toUnion Nothing = Z (I ())
  toUnion (Just x) = S (Z (I x))

  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of

instance
  (SwaggerMethod method, IsResponseList as) =>
  S.HasSwagger (MultiVerb method as r)
  where
  toSwagger _ =
    mempty
      & S.definitions <>~ defs
      & S.paths
        . at "/"
        ?~ ( mempty
               & method
                 ?~ ( mempty
                        & S.produces ?~ S.MimeList (nubOrd cs)
                        & S.responses .~ foldr addResponse mempty responses
                    )
           )
    where
      method = S.swaggerMethod (Proxy @method)
      cs = mconcat (responseListContentTypes @as)
      (defs, responses) = S.runDeclare (responseListSwagger @as) mempty
      addResponse :: ResponseSwagger -> S.Responses -> S.Responses
      addResponse response =
        at (statusCode (rsStatus response))
          .~ (Just . S.Inline)
            ( mempty
                & S.description .~ rsDescription response
                & S.schema .~ rsSchema response
                & S.headers .~ rsHeaders response
            )

roResponse :: RenderOutput -> Wai.Response
roResponse ro = Wai.responseLBS (roStatus ro) (roHeaders ro) (roBody ro)

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

instance
  (IsResponseList as, AsUnion as r, ReflectMethod method) =>
  HasServer (MultiVerb method as r) ctx
  where
  type ServerT (MultiVerb method as r) m = m r

  hoistServerWithContext _ _ nt s = nt s

  route ::
    forall env.
    Proxy (MultiVerb method as r) ->
    Context ctx ->
    Delayed env (Handler r) ->
    Router env
  route _ _ action = leafRouter $ \env req k -> do
    let action' :: Delayed env (Handler r)
        action' = action `addMethodCheck` methodCheck method req
    -- FUTUREWORK: add eager content type check here?
    (>>= k) . runRouteResultT . hoistRouteResult runResourceT $ do
      handler <- RouteResultT $ runDelayed action' env req
      output <- toUnion @as <$> handlerToRouteResult handler
      let i = index_NS output
          cs = responseListContentTypes @as !! i
      resp <- case matchContentType cs (getAcceptHeader req) of
        Nothing -> RouteResultT . pure $ FailFatal err406
        Just (j, c) -> pure (roAddContentType c (responseListRender @as output !! j))
      let resp'
            | allowedMethodHead method req = resp {roBody = mempty}
            | otherwise = resp
      pure $ roResponse resp'
    where
      method = reflectMethod (Proxy @method)

      matchContentType :: [M.MediaType] -> AcceptHeader -> Maybe (Int, M.MediaType)
      matchContentType cs (AcceptHeader h) = do
        c <- M.matchAccept cs h
        i <- elemIndex c cs
        pure (i, c)

-- copied from Servant.Client.Core.HasClient
getResponseContentType :: RunClient m => Response -> m M.MediaType
getResponseContentType response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application" M.// "octet-stream"
    Just t -> case M.parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

-- pick the first response from as that has the specified content type and
-- status
class DeconstructResponse (as :: [*]) where
  deconstructResponse ::
    M.MediaType ->
    Status ->
    LByteString ->
    Either String (Union (ResponseTypes as))

instance DeconstructResponse '[] where
  deconstructResponse _ _ _ =
    Left "Unexpected status and/or content type"

instance
  ( AllMimeUnrender (ResponseContentTypes a) (ResponseType a),
    KnownStatus (ResponseStatus a),
    DeconstructResponse as
  ) =>
  DeconstructResponse (a ': as)
  where
  deconstructResponse c s b = case elemIndex c (responseContentTypes @a) of
    Just i
      | s == responseStatus @a ->
        fmap (Z . I) (snd (allMimeUnrender (Proxy @(ResponseContentTypes a)) !! i) b)
    _ -> fmap S (deconstructResponse @as c s b)

-- FUTUREWORK: add tests for client support
instance
  ( IsResponseList as,
    DeconstructResponse as,
    ReflectMethod method,
    AsUnion as r,
    RunClient m
  ) =>
  HasClient m (MultiVerb method as r)
  where
  type Client m (MultiVerb method as r) = m r

  clientWithRoute _ _ req = do
    response <-
      runRequestAcceptStatus
        (Just (responseListStatuses @as))
        req
          { requestMethod = method,
            requestAccept = Seq.fromList accept
          }

    c <- getResponseContentType response
    either (throwClientError . err response) (pure . fromUnion @as) $
      deconstructResponse @as c (responseStatusCode response) (responseBody response)
    where
      accept = mconcat (responseListContentTypes @as)
      method = reflectMethod (Proxy :: Proxy method)
      err :: Response -> String -> ClientError
      err response e = DecodeFailure (Text.pack e) response

  hoistClientMonad _ _ f = f
