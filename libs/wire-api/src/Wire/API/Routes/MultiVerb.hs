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
    RespondEmpty,
    WithHeaders,
    DescHeader,
    AsHeaders (..),
    AsUnion (..),
    IsResponse (..),
    IsResponseList (..),
  )
where

import Control.Applicative
import Control.Lens hiding (Context)
import qualified Data.ByteString.Lazy as LBS
import Data.Containers.ListUtils
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Proxy
import Data.SOP
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

type Declare = S.Declare (S.Definitions S.Schema)

-- | A type to describe a 'MultiVerb' response.
--
-- Includes status code, description, and return type.
data Respond (s :: Nat) (desc :: Symbol) (a :: *)

-- | A type to describe a 'MultiVerb' response with an empty body.
--
-- Includes status code and description.
data RespondEmpty (s :: Nat) (desc :: Symbol)

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

-- | The result of parsing a response as a union alternative of type 'a'.
--
-- 'StatusMismatch' indicates that the response does not refer to the given
-- alternative, because the status code does not match the one produced by that
-- alternative.
--
-- 'UnrenderError' and 'UnrenderSuccess' represent respectively a failing and
-- successful parse of the response body as a value of type 'a'.
--
-- The 'UnrenderResult' type constructor has monad and alternative instances
-- corresponding to those of 'Either (Maybe (Last String)) a'.
data UnrenderResult a = StatusMismatch | UnrenderError String | UnrenderSuccess a
  deriving (Eq, Show, Functor)

instance Applicative UnrenderResult where
  pure = UnrenderSuccess
  (<*>) = ap

instance Monad UnrenderResult where
  return = pure
  StatusMismatch >>= _ = StatusMismatch
  UnrenderError e >>= _ = UnrenderError e
  UnrenderSuccess x >>= f = f x

instance Alternative UnrenderResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus UnrenderResult where
  mzero = StatusMismatch
  mplus StatusMismatch m = m
  mplus (UnrenderError e) StatusMismatch = UnrenderError e
  mplus (UnrenderError _) m = m
  mplus m@(UnrenderSuccess _) _ = m

class IsSwaggerResponse a where
  responseSwagger :: Declare ResponseSwagger

class IsResponse cs a where
  type ResponseType a :: *
  type ResponseStatus a :: Nat

  responseRender :: AcceptHeader -> ResponseType a -> Maybe RenderOutput
  responseUnrender :: M.MediaType -> RenderOutput -> UnrenderResult (ResponseType a)

instance (AllMimeRender cs a, AllMimeUnrender cs a, KnownStatus s) => IsResponse cs (Respond s desc a) where
  type ResponseType (Respond s desc a) = a
  type ResponseStatus (Respond s desc a) = s

  responseRender (AcceptHeader acc) x =
    M.mapAcceptMedia (map (uncurry mkRenderOutput) (allMimeRender (Proxy @cs) x)) acc
    where
      mkRenderOutput :: M.MediaType -> LByteString -> (M.MediaType, RenderOutput)
      mkRenderOutput c body =
        (c,) . roAddContentType c $
          RenderOutput
            { roStatus = statusVal (Proxy @s),
              roBody = body,
              roHeaders = []
            }

  responseUnrender c output = do
    guard (roStatus output == statusVal (Proxy @s))
    let results = allMimeUnrender (Proxy @cs)
    case lookup c results of
      Nothing -> empty
      Just f -> either UnrenderError UnrenderSuccess (f (roBody output))

instance
  (KnownStatus s, KnownSymbol desc, S.ToSchema a) =>
  IsSwaggerResponse (Respond s desc a)
  where
  responseSwagger =
    ResponseSwagger desc status mempty . Just
      <$> S.declareSchemaRef (Proxy @a)
    where
      desc = Text.pack (symbolVal (Proxy @desc))
      status = statusVal (Proxy @s)

instance KnownStatus s => IsResponse cs (RespondEmpty s desc) where
  type ResponseType (RespondEmpty s desc) = ()
  type ResponseStatus (RespondEmpty s desc) = s

  responseRender _ _ =
    Just
      RenderOutput
        { roStatus = statusVal (Proxy @s),
          roBody = mempty,
          roHeaders = []
        }

  responseUnrender _ output =
    guard
      ( roStatus output == statusVal (Proxy @s)
          && LBS.null (roBody output)
      )

instance (KnownStatus s, KnownSymbol desc) => IsSwaggerResponse (RespondEmpty s desc) where
  responseSwagger = pure $ ResponseSwagger desc status mempty Nothing
    where
      desc = Text.pack (symbolVal (Proxy @desc))
      status = statusVal (Proxy @s)

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

-- convert a list of 'Header's to a list of 'Servant.Header's
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
    BuildHeadersTo (ServantHeaders hs),
    AllToResponseHeader hs,
    IsResponse cs r
  ) =>
  IsResponse cs (WithHeaders hs a r)
  where
  type ResponseType (WithHeaders hs a r) = a
  type ResponseStatus (WithHeaders hs a r) = ResponseStatus r

  responseRender acc x =
    fmap addHeaders
      . responseRender @cs @r acc
      . getResponse
      $ h
    where
      h = toHeaders @(ServantHeaders hs) x
      addHeaders r = r {roHeaders = roHeaders r ++ getHeaders h}

  responseUnrender c output = do
    x <- responseUnrender @cs @r c output
    let headers = Headers x (buildHeadersTo @(ServantHeaders hs) (roHeaders output))
    pure (fromHeaders headers)

instance
  (AllToResponseHeader hs, IsSwaggerResponse r) =>
  IsSwaggerResponse (WithHeaders hs a r)
  where
  responseSwagger =
    fmap
      (\rs -> rs {rsHeaders = toAllResponseHeaders (Proxy @hs)})
      (responseSwagger @r)

class IsSwaggerResponseList as where
  responseListSwagger :: Declare [ResponseSwagger]

class IsResponseList cs as where
  type ResponseTypes as :: [*]

  responseListRender :: AcceptHeader -> Union (ResponseTypes as) -> Maybe RenderOutput
  responseListUnrender :: M.MediaType -> RenderOutput -> UnrenderResult (Union (ResponseTypes as))

  responseListStatuses :: [Status]

instance IsResponseList cs '[] where
  type ResponseTypes '[] = '[]

  responseListRender _ x = case x of
  responseListUnrender _ _ = empty
  responseListStatuses = []

instance IsSwaggerResponseList '[] where
  responseListSwagger = pure []

instance
  ( IsResponse cs a,
    IsResponseList cs as,
    KnownStatus (ResponseStatus a)
  ) =>
  IsResponseList cs (a ': as)
  where
  type ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

  responseListRender acc (Z (I x)) = responseRender @cs @a acc x
  responseListRender acc (S x) = responseListRender @cs @as acc x

  responseListUnrender c output =
    Z . I <$> responseUnrender @cs @a c output
      <|> S <$> responseListUnrender @cs @as c output

  responseListStatuses = statusVal (Proxy @(ResponseStatus a)) : responseListStatuses @cs @as

instance
  (IsSwaggerResponse a, IsSwaggerResponseList as) =>
  IsSwaggerResponseList (a ': as)
  where
  responseListSwagger = (:) <$> responseSwagger @a <*> responseListSwagger @as

-- | This type can be used in Servant to produce an endpoint which can return
-- multiple values with various content types and status codes. It is similar to
-- 'UVerb' and behaves similarly, but it has some important differences:
--
--  * Descriptions and statuses can be attached to individual responses without
--    using wrapper types and without affecting the handler return type.
--  * The return type of the handler can be decoupled from the types of the
--    individual responses. One can use a 'Union' type just like for 'UVerb',
--    but 'MultiVerb' also supports using an arbitrary type with an 'AsUnion'
--    instance.
--  * Headers can be attached to individual responses, also without affecting
--    the handler return type.
data MultiVerb (method :: StdMethod) (cs :: [*]) (as :: [*]) (r :: *)

-- | This class is used to convert a handler return type to a union type
-- including all possible responses of a 'MultiVerb' endpoint.
class AsUnion (as :: [*]) (r :: *) where
  toUnion :: r -> Union (ResponseTypes as)
  fromUnion :: Union (ResponseTypes as) -> r

instance rs ~ ResponseTypes as => AsUnion as (Union rs) where
  toUnion = id
  fromUnion = id

instance
  AsUnion
    '[ RespondEmpty s1 desc1,
       RespondEmpty s2 desc2
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
    '[RespondEmpty s1 desc1, r2]
    (Maybe a)
  where
  toUnion Nothing = Z (I ())
  toUnion (Just x) = S (Z (I x))

  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of

instance
  (SwaggerMethod method, IsSwaggerResponseList as, AllMime cs) =>
  S.HasSwagger (MultiVerb method cs as r)
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
      cs = allMime (Proxy @cs)
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

instance
  (AllMime cs, IsResponseList cs as, AsUnion as r, ReflectMethod method) =>
  HasServer (MultiVerb method cs as r) ctx
  where
  type ServerT (MultiVerb method cs as r) m = m r

  hoistServerWithContext _ _ f = f

  route ::
    forall env.
    Proxy (MultiVerb method cs as r) ->
    Context ctx ->
    Delayed env (Handler r) ->
    Router env
  route _ _ action = leafRouter $ \env req k -> do
    let acc = getAcceptHeader req
        action' =
          action `addMethodCheck` methodCheck method req
            `addMethodCheck` acceptCheck (Proxy @cs) acc
    runAction action' env req k $ \output -> do
      let mresp = responseListRender @cs @as acc (toUnion @as output)
      resp' <- case mresp of
        Nothing -> FailFatal err406
        Just resp
          | allowedMethodHead method req -> pure $ resp {roBody = mempty}
          | otherwise -> pure resp
      pure (roResponse resp')
    where
      method = reflectMethod (Proxy @method)

-- taken from Servant.Client.Core.HasClient
getResponseContentType :: RunClient m => Response -> m M.MediaType
getResponseContentType response =
  case lookup "Content-Type" (toList (responseHeaders response)) of
    Nothing -> return $ "application" M.// "octet-stream"
    Just t -> case M.parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

-- FUTUREWORK: add tests for client support
instance
  ( IsResponseList cs as,
    AllMime cs,
    ReflectMethod method,
    AsUnion as r,
    RunClient m
  ) =>
  HasClient m (MultiVerb method cs as r)
  where
  type Client m (MultiVerb method cs as r) = m r

  clientWithRoute _ _ req = do
    response <-
      runRequestAcceptStatus
        (Just (responseListStatuses @cs @as))
        req
          { requestMethod = method,
            requestAccept = Seq.fromList accept
          }

    c <- getResponseContentType response
    let output =
          RenderOutput
            { roBody = responseBody response,
              roHeaders = toList (responseHeaders response),
              roStatus = responseStatusCode response
            }

    unless (any (M.matches c) accept) $ do
      throwClientError $ UnsupportedContentType c response
    case responseListUnrender @cs @as c output of
      StatusMismatch -> throwClientError (DecodeFailure "Status mismatch" response)
      UnrenderError e -> throwClientError (DecodeFailure (Text.pack e) response)
      UnrenderSuccess x -> pure (fromUnion @as x)
    where
      accept = allMime (Proxy @cs)
      method = reflectMethod (Proxy @method)

  hoistClientMonad _ _ f = f
