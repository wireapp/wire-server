{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.Middleware
    ( PathTemplate
    , withPathTemplate
    , duration
    , requestCounter
    , module Data.Metrics
    ) where

import Imports
import Data.Metrics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Route.Tree (Tree)
import Network.Wai.Internal (Response (ResponseRaw))
import System.Clock

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.Wai.Route.Tree as Tree

newtype PathTemplate = PathTemplate Text

withPathTemplate :: Tree a -> (PathTemplate -> Middleware) -> Middleware
withPathTemplate t f app r k = f (fromMaybe def tmp) app r k
  where
    def = PathTemplate "N/A"
    tmp = PathTemplate
        . T.decodeUtf8
        . Tree.path <$> Tree.lookup t (Tree.segments $ rawPathInfo r)

duration :: Int -> Int -> Metrics -> PathTemplate -> Middleware
duration start len m  (PathTemplate t) f rq k = do
    st <- getTime Monotonic
    rs <- f rq k
    ed <- getTime Monotonic
    let p = mkPath [t, methodName rq, "time"]
    bucketsIncr start len (timeSpecAsMilliSecs $ ed `diffTimeSpec` st) p m
    return rs

-- Count Requests and their status code.
--
-- [Note [Raw Response]]:
--
-- We ignore the status code of raw responses which are returned after
-- websocket communication ends because there is no meaningful status code
-- to ask for. WAI uses the fallback response status code (i.e. 500) which
-- is only used in servers which do not support raw responses (i.e. not
-- Warp).
requestCounter :: Metrics -> PathTemplate -> Middleware
requestCounter m (PathTemplate t) f rq k = f rq onResponse
  where
    onResponse rs@(ResponseRaw _ _) = do -- See Note [Raw Response]
        counterIncr (path "net.requests") m
        k rs
    onResponse rs = do
        counterIncr (path "net.requests") m
        counterIncr (mkPath [t, methodName rq, "status", code rs]) m
        k rs

mkPath :: [Text] -> Path
mkPath = path . mconcat . intersperse "." . ("net.resources":)
{-# INLINE mkPath #-}

code :: Response -> Text
code = T.pack . show . statusCode . responseStatus
{-# INLINE code #-}

methodName :: Request -> Text
methodName = T.decodeUtf8 . requestMethod
{-# INLINE methodName #-}

timeSpecAsMilliSecs :: TimeSpec -> Word
timeSpecAsMilliSecs t = fromIntegral (sec t * 1000 + nsec t `div` 1000000)
