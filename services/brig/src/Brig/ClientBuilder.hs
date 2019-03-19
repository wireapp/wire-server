{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Brig.ClientBuilder  where

import Imports
import Network.Wai (defaultRequest, Response, ResponseReceived)
import Network.Wai.Predicate hiding (setStatus, result, and, (#))
import qualified Network.Wai.Predicate as P
import qualified Network.Wai.Predicate.Request as P
import Network.Wai.Routing
import Network.Wai.Routing.Request
import Data.CaseInsensitive
import Brig.API.Handler
import Network.HTTP.Types
import Control.Arrow((&&&))
import Data.Proxy
import GHC.TypeLits
import Bilge.Request as B
import Data.ByteString.Char8 (pack)

data HeaderP s = HeaderP ByteString
data QueryP s = QueryP (Maybe ByteString)

class ApplyParam params where
    applyParam :: params -> Request -> Request

instance (ApplyParam p, ApplyParam ps) => ApplyParam (p ::: ps) where
    applyParam (p ::: ps) = applyParam ps . applyParam p

instance (KnownSymbol s) => ApplyParam (HeaderP s) where
    applyParam (HeaderP v) = B.header (mk . pack $  symbolVal (Proxy @s)) v

instance (KnownSymbol s) => ApplyParam (QueryP s) where
    applyParam (QueryP v) = B.query [(pack $ symbolVal (Proxy @s), v)]

data ClientMeta = CM { mParams :: ReqParams
                     } deriving Show

get' :: Monad m
     => ByteString
     -> (a -> Continue m -> m ResponseReceived)
     -> (ReqParams, Predicate RoutingReq P.Error a)
     -> Routes ClientMeta m ()
get' pth handler (params, preds) = do
    get pth handler preds
    attach (CM { mParams = params })

simpleHandler :: (JSON ::: Text) -> m Response
simpleHandler (_ ::: _) = undefined

testRoute :: Routes ClientMeta IO ()
testRoute = do
    get' "/calls/config" (continue simpleHandler) $
        accept' "application" "json"
        .&.. header' "Z-User"
    attach (CM { mParams = [] })

clientInfo :: [Meta ClientMeta]
clientInfo = examine testRoute

data OnRequest = Query String | Header String | ContentType String deriving Show
type ReqParams = [OnRequest]

accept' :: P.HasHeaders r => ByteString -> ByteString -> (ReqParams, Predicate r Error (Media (t :: Symbol) (s :: Symbol)))
accept' a b = (pure $ ContentType a b, accept a b)

header' :: P.HasHeaders r => HeaderName -> (ReqParams, Predicate r Error Text)
header' = (pure . Header &&& header)

(.&..) :: (ReqParams, Predicate a f t) -> (ReqParams, Predicate a f t') -> (ReqParams, Predicate a f (t ::: t'))
(.&..) = liftA2 (.&.)
