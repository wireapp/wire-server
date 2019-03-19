{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Brig.ClientBuilder  where

import Imports
import Network.Wai (defaultRequest, Request, Response, ResponseReceived)
import Network.Wai.Predicate hiding (setStatus, result, and, (#))
import qualified Network.Wai.Predicate as P
import qualified Network.Wai.Predicate.Request as P
import Network.Wai.Routing
import Network.Wai.Routing.Request
import Data.CaseInsensitive
import Brig.API.Handler
import Network.HTTP.Types
import Control.Arrow((&&&))
import GHC.TypeLits

newtype Tagged t a = T a

type family CallFunc params where
    CallFunc (p ::: ps) = AsParam p -> CallFunc ps
    CallFunc () = Request -> Request
    CallFunc r = r -> (Request -> Request)

type family AsParam p where
    AsParam (HeaderP s) = Tagged (HeaderP s) String
    AsParam (QueryP s) = Tagged (QueryP s) String

data HeaderP s
data QueryP s

class ToClient params where
    toClient :: CallFunc params

class ApplyParam p where
    applyParam :: AsParam p -> (Request -> Request)

instance {-# OVERLAPPING #-}  (ApplyParam p, ToClient ps) => ToClient (p ::: ps) where
    toClient v = toClient @ps $ applyParam @p v

instance {-# OVERLAPPABLE #-} (ApplyParam p, (CallFunc p) ~ (AsParam p -> (Request -> Request))) => ToClient p where
    toClient v = applyParam @p v $ defaultRequest


-- data ClientMeta = CM { mParams :: ReqParams
                     -- } deriving Show

-- get' :: Monad m
--      => ByteString
--      -> (a -> Continue m -> m ResponseReceived)
--      -> (ReqParams, Predicate RoutingReq P.Error a)
--      -> Routes ClientMeta m ()
-- get' pth handler (params, preds) = do
--     get pth handler preds
--     attach (CM { mParams = params })

-- simpleHandler :: (JSON ::: Text) -> m Response
-- simpleHandler (_ ::: _) = undefined

-- testRoute :: Routes ClientMeta IO ()
-- testRoute = do
--     get' "/calls/config" (continue simpleHandler) $
--         accept' "application" "json"
--         .&.. header' "Z-User"
--     attach (CM { mParams = [] })

-- clientInfo :: [Meta ClientMeta]
-- clientInfo = examine testRoute

-- data OnRequest = Query String | Header String | ContentType String deriving Show
-- type ReqParams = [OnRequest]

-- accept' :: P.HasHeaders r => ByteString -> ByteString -> (ReqParams, Predicate r Error (Media (t :: Symbol) (s :: Symbol)))
-- accept' a b = (pure $ ContentType a b, accept a b)

-- header' :: P.HasHeaders r => HeaderName -> (ReqParams, Predicate r Error Text)
-- header' = (pure . Header &&& header)

-- (.&..) :: (ReqParams, Predicate a f t) -> (ReqParams, Predicate a f t') -> (ReqParams, Predicate a f (t ::: t'))
-- (.&..) = liftA2 (.&.)
