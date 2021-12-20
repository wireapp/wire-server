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
    eitherToUnion,
    eitherFromUnion,
    AsConstructor (..),
    GenericAsConstructor (..),
    GenericAsUnion (..),
    ResponseType,
    IsResponse (..),
    IsSwaggerResponse (..),
    combineResponseSwagger,
    RenderOutput (..),
    roAddContentType,
    roResponse,
    ResponseTypes,
    IsResponseList (..),
  )
where

import Control.Applicative
import Control.Lens hiding (Context)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Containers.ListUtils
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Metrics.Servant
import Data.Proxy
import Data.SOP
import qualified Data.Sequence as Seq
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.TypeLits
import Generics.SOP as GSOP
import Imports
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (HeaderName, hContentType)
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Status (KnownStatus (..))
import Servant.Client
import Servant.Client.Core hiding (addHeader)
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
  responseSwagger :: Declare S.Response

type family ResponseType a :: *

class IsResponse cs a where
  type ResponseStatus a :: Nat

  responseRender :: AcceptHeader -> ResponseType a -> Maybe RenderOutput
  responseUnrender :: M.MediaType -> RenderOutput -> UnrenderResult (ResponseType a)

type instance ResponseType (Respond s desc a) = a

instance (AllMimeRender cs a, AllMimeUnrender cs a, KnownStatus s) => IsResponse cs (Respond s desc a) where
  type ResponseStatus (Respond s desc a) = s

  -- Note: here it seems like we are rendering for all possible content types,
  -- only to choose the correct one afterwards. However, render results besides the
  -- one picked by 'M.mapAcceptMedia' are not evaluated, and therefore nor are the
  -- corresponding rendering functions.
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
  responseSwagger = do
    ref <- S.declareSchemaRef (Proxy @a)
    pure $
      mempty
        & S.description .~ Text.pack (symbolVal (Proxy @desc))
        & S.schema ?~ ref

type instance ResponseType (RespondEmpty s desc) = ()

instance KnownStatus s => IsResponse cs (RespondEmpty s desc) where
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
  responseSwagger =
    pure $
      mempty
        & S.description .~ Text.pack (symbolVal (Proxy @desc))

-- | This type adds response headers to a 'MultiVerb' response.
--
-- Type variables:
--  * @hs@: type-level list of headers
--  * @a@: return type (with headers)
--  * @r@: underlying response (without headers)
data WithHeaders (hs :: [*]) (a :: *) (r :: *)

-- | This is used to convert a response containing headers to a custom type
-- including the information in the headers.
class AsHeaders xs a b where
  fromHeaders :: (NP I xs, a) -> b
  toHeaders :: b -> (NP I xs, a)

-- single-header empty response
instance AsHeaders '[a] () a where
  toHeaders a = (I a :* Nil, ())
  fromHeaders = unI . hd . fst

data DescHeader (name :: Symbol) (desc :: Symbol) (a :: *)

class ServantHeaders hs xs | hs -> xs where
  constructHeaders :: NP I xs -> [HTTP.Header]
  extractHeaders :: [HTTP.Header] -> Maybe (NP I xs)

instance ServantHeaders '[] '[] where
  constructHeaders Nil = []
  extractHeaders _ = Just Nil

headerName :: forall name. KnownSymbol name => HTTP.HeaderName
headerName =
  CI.mk
    . Text.encodeUtf8
    . Text.pack
    $ symbolVal (Proxy @name)

instance
  ( KnownSymbol name,
    ServantHeader h name x,
    ToHttpApiData x,
    FromHttpApiData x,
    ServantHeaders hs xs
  ) =>
  ServantHeaders (h ': hs) (x ': xs)
  where
  constructHeaders (I x :* xs) =
    (headerName @name, toHeader x) :
    constructHeaders @hs xs

  extractHeaders hs = do
    let name = headerName @name
        (hs0, hs1) = partition (\(h, _) -> h == name) hs
    x <- case hs0 of
      [] -> empty
      ((_, h) : _) -> either (const empty) pure (parseHeader h)
    xs <- extractHeaders @hs hs1
    pure (I x :* xs)

class ServantHeader h (name :: Symbol) x | h -> name x

instance ServantHeader (Header' mods name x) name x

instance ServantHeader (DescHeader name desc x) name x

instance
  (KnownSymbol name, KnownSymbol desc, S.ToParamSchema a) =>
  ToResponseHeader (DescHeader name desc a)
  where
  toResponseHeader _ = (name, S.Header (Just desc) sch)
    where
      name = Text.pack (symbolVal (Proxy @name))
      desc = Text.pack (symbolVal (Proxy @desc))
      sch = S.toParamSchema (Proxy @a)

type instance ResponseType (WithHeaders hs a r) = a

instance
  ( AsHeaders xs (ResponseType r) a,
    ServantHeaders hs xs,
    IsResponse cs r
  ) =>
  IsResponse cs (WithHeaders hs a r)
  where
  type ResponseStatus (WithHeaders hs a r) = ResponseStatus r

  responseRender acc x = fmap addHeaders $ responseRender @cs @r acc y
    where
      (hs, y) = toHeaders @xs x
      addHeaders r = r {roHeaders = roHeaders r ++ constructHeaders @hs hs}

  responseUnrender c output = do
    x <- responseUnrender @cs @r c output
    case extractHeaders @hs (roHeaders output) of
      Nothing -> UnrenderError "Failed to parse headers"
      Just hs -> pure $ fromHeaders @xs (hs, x)

instance
  (AllToResponseHeader hs, IsSwaggerResponse r) =>
  IsSwaggerResponse (WithHeaders hs a r)
  where
  responseSwagger =
    fmap
      (S.headers .~ toAllResponseHeaders (Proxy @hs))
      (responseSwagger @r)

class IsSwaggerResponseList as where
  responseListSwagger :: Declare (InsOrdHashMap S.HttpStatusCode S.Response)

type family ResponseTypes (as :: [*]) where
  ResponseTypes '[] = '[]
  ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

class IsResponseList cs as where
  responseListRender :: AcceptHeader -> Union (ResponseTypes as) -> Maybe RenderOutput
  responseListUnrender :: M.MediaType -> RenderOutput -> UnrenderResult (Union (ResponseTypes as))

  responseListStatuses :: [Status]

instance IsResponseList cs '[] where
  responseListRender _ x = case x of
  responseListUnrender _ _ = empty
  responseListStatuses = []

instance IsSwaggerResponseList '[] where
  responseListSwagger = pure mempty

instance
  ( IsResponse cs a,
    IsResponseList cs as,
    KnownStatus (ResponseStatus a)
  ) =>
  IsResponseList cs (a ': as)
  where
  responseListRender acc (Z (I x)) = responseRender @cs @a acc x
  responseListRender acc (S x) = responseListRender @cs @as acc x

  responseListUnrender c output =
    Z . I <$> responseUnrender @cs @a c output
      <|> S <$> responseListUnrender @cs @as c output

  responseListStatuses = statusVal (Proxy @(ResponseStatus a)) : responseListStatuses @cs @as

instance
  ( IsSwaggerResponse a,
    KnownNat (ResponseStatus a),
    IsSwaggerResponseList as
  ) =>
  IsSwaggerResponseList (a ': as)
  where
  responseListSwagger =
    InsOrdHashMap.insertWith
      combineResponseSwagger
      (fromIntegral (natVal (Proxy @(ResponseStatus a))))
      <$> responseSwagger @a
      <*> responseListSwagger @as

combineResponseSwagger :: S.Response -> S.Response -> S.Response
combineResponseSwagger r1 r2 =
  r1
    & S.description <>~ ("\n\n" <> r2 ^. S.description)
    & S.schema . _Just . S._Inline %~ flip combineSwaggerSchema (r2 ^. S.schema . _Just . S._Inline)

combineSwaggerSchema :: S.Schema -> S.Schema -> S.Schema
combineSwaggerSchema s1 s2
  -- if they are both errors, merge label enums
  | notNullOf (S.properties . ix "code") s1
      && notNullOf (S.properties . ix "code") s2 =
    s1 & S.properties . ix "label" . S._Inline . S.enum_ . _Just
      <>~ (s2 ^. S.properties . ix "label" . S._Inline . S.enum_ . _Just)
  | otherwise = s1

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
--
-- Any glue code necessary to convert application types to and from the
-- canonical 'Union' type corresponding to a 'MultiVerb' endpoint should be
-- packaged into an 'AsUnion' instance.
class AsUnion (as :: [*]) (r :: *) where
  toUnion :: r -> Union (ResponseTypes as)
  fromUnion :: Union (ResponseTypes as) -> r

-- | Unions can be used directly as handler return types using this trivial
-- instance.
instance rs ~ ResponseTypes as => AsUnion as (Union rs) where
  toUnion = id
  fromUnion = id

-- | A handler with a single response.
instance (ResponseType r ~ a) => AsUnion '[r] a where
  toUnion = Z . I
  fromUnion = unI . unZ

_foo :: Union '[Int]
_foo = toUnion @'[Respond 200 "test" Int] @Int 3

class InjectAfter as bs where
  injectAfter :: Union bs -> Union (as .++ bs)

instance InjectAfter '[] bs where
  injectAfter = id

instance InjectAfter as bs => InjectAfter (a ': as) bs where
  injectAfter = S . injectAfter @as @bs

class InjectBefore as bs where
  injectBefore :: Union as -> Union (as .++ bs)

instance InjectBefore '[] bs where
  injectBefore x = case x of

instance InjectBefore as bs => InjectBefore (a ': as) bs where
  injectBefore (Z x) = Z x
  injectBefore (S x) = S (injectBefore @as @bs x)

eitherToUnion ::
  forall as bs a b.
  (InjectAfter as bs, InjectBefore as bs) =>
  (a -> Union as) ->
  (b -> Union bs) ->
  (Either a b -> Union (as .++ bs))
eitherToUnion f _ (Left a) = injectBefore @as @bs (f a)
eitherToUnion _ g (Right b) = injectAfter @as @bs (g b)

class EitherFromUnion as bs where
  eitherFromUnion ::
    (Union as -> a) ->
    (Union bs -> b) ->
    (Union (as .++ bs) -> Either a b)

instance EitherFromUnion '[] bs where
  eitherFromUnion _ g = Right . g

instance EitherFromUnion as bs => EitherFromUnion (a ': as) bs where
  eitherFromUnion f _ (Z x) = Left (f (Z x))
  eitherFromUnion f g (S x) = eitherFromUnion @as @bs (f . S) g x

-- | This class can be instantiated to get automatic derivation of 'AsUnion'
-- instances via 'GenericAsUnion'. The idea is that one has to make sure that for
-- each response @r@ in a 'MultiVerb' endpoint, there is an instance of
-- @AsConstructor xs r@ for some @xs@, and that the list @xss@ of all the
-- corresponding @xs@ is equal to 'GSOP.Code' of the handler type. Then one can
-- write:
-- @
--   type Responses = ...
--   data Result = ...
--     deriving stock (Generic)
--     deriving (AsUnion Responses) via (GenericAsUnion Responses Result)
--
--   instance GSOP.Generic Result
-- @
-- and get an 'AsUnion' instance for free.
--
-- There are a few predefined instances for constructors taking a single type
-- corresponding to a simple response, and for empty responses, but in more
-- general cases one either has to define an 'AsConstructor' instance by hand,
-- or derive it via 'GenericAsConstructor'.
class AsConstructor xs r where
  toConstructor :: ResponseType r -> NP I xs
  fromConstructor :: NP I xs -> ResponseType r

class AsConstructors xss rs where
  toSOP :: Union (ResponseTypes rs) -> SOP I xss
  fromSOP :: SOP I xss -> Union (ResponseTypes rs)

instance AsConstructors '[] '[] where
  toSOP x = case x of
  fromSOP x = case x of

instance AsConstructor '[a] (Respond code desc a) where
  toConstructor x = I x :* Nil
  fromConstructor = unI . hd

instance AsConstructor '[] (RespondEmpty code desc) where
  toConstructor _ = Nil
  fromConstructor _ = ()

newtype GenericAsConstructor r = GenericAsConstructor r

type instance ResponseType (GenericAsConstructor r) = ResponseType r

instance
  (GSOP.Code (ResponseType r) ~ '[xs], GSOP.Generic (ResponseType r)) =>
  AsConstructor xs (GenericAsConstructor r)
  where
  toConstructor = unZ . unSOP . GSOP.from
  fromConstructor = GSOP.to . SOP . Z

instance
  (AsConstructor xs r, AsConstructors xss rs) =>
  AsConstructors (xs ': xss) (r ': rs)
  where
  toSOP (Z (I x)) = SOP . Z $ toConstructor @xs @r x
  toSOP (S x) = SOP . S . unSOP $ toSOP @xss @rs x

  fromSOP (SOP (Z x)) = Z (I (fromConstructor @xs @r x))
  fromSOP (SOP (S x)) = S (fromSOP @xss @rs (SOP x))

-- | This type is meant to be used with @deriving via@ in order to automatically
-- generate an 'AsUnion' instance using 'Generics.SOP'.
--
-- See 'AsConstructor' for more information and examples.
newtype GenericAsUnion rs a = GenericAsUnion a

instance
  (GSOP.Code a ~ xss, GSOP.Generic a, AsConstructors xss rs) =>
  AsUnion rs (GenericAsUnion rs a)
  where
  toUnion (GenericAsUnion x) = fromSOP @xss @rs (GSOP.from x)
  fromUnion = GenericAsUnion . GSOP.to . toSOP @xss @rs

-- | A handler for a pair of empty responses can be implemented simply by
-- returning a boolean value. The convention is that the "failure" case, normally
-- represented by 'False', corresponds to the /first/ response.
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

-- | A handler for a pair of responses where the first is empty can be
-- implemented simply by returning a 'Maybe' value. The convention is that the
-- "failure" case, normally represented by 'Nothing', corresponds to the /first/
-- response.
instance
  AsUnion
    '[RespondEmpty s1 desc1, Respond s2 desc2 a]
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
                        & S.responses . S.responses .~ fmap S.Inline responses
                    )
           )
    where
      method = S.swaggerMethod (Proxy @method)
      cs = allMime (Proxy @cs)
      (defs, responses) = S.runDeclare (responseListSwagger @as) mempty

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
            `addAcceptCheck` acceptCheck (Proxy @cs) acc
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

instance RoutesToPaths (MultiVerb method cs as r) where
  getRoutes = []
