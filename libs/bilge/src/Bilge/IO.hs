{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bilge.IO
    ( -- * Convenience API
      HttpT        (..)
    , Http
    , MonadHttp    (..)
    , Debug        (..)
    , runHttpT
    , http
    , httpLbs
    , httpDebug
    , get
    , get'
    , put
    , put'
    , post
    , post'
    , head
    , head'
    , delete
    , delete'
    , options
    , options'
    , trace
    , trace'
    , patch
    , patch'
    , consumeBody

    -- * Re-exports
    , ManagerSettings (..)
    , withResponse
    , Manager
    , newManager
    , withManager
    , defaultManagerSettings
    , BodyReader
    , brRead
    , brConsume
    , HttpException (..)
    ) where

import Prelude hiding (head)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.HTTP.Client hiding (method, httpLbs)
import Network.HTTP.Types
import Bilge.Request
import Bilge.Response

import qualified Data.ByteString.Lazy as Lazy

-- | Debug settings may cause debug information to be printed to stdout.
data Debug
    = Head -- ^ Print HTTP request/response header.
    | Full -- ^ Like 'Head' but also print the response body.
    deriving (Eq, Ord, Show, Read, Enum)

type Http a = HttpT IO a

newtype HttpT m a = HttpT
    { unwrap :: ReaderT Manager m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadTrans
               , MonadReader Manager
               )

class MonadHttp m where
    getManager :: m Manager

instance Monad m => MonadHttp (HttpT m) where
    getManager = HttpT ask

instance MonadBase IO (HttpT IO) where
    liftBase = liftIO

instance MonadTransControl HttpT where
    type StT HttpT a = StT (ReaderT Manager) a
    liftWith = defaultLiftWith HttpT unwrap
    restoreT = defaultRestoreT HttpT

instance MonadBaseControl IO (HttpT IO) where
    type StM (HttpT IO) a = ComposeSt HttpT IO a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

runHttpT :: Monad m => Manager -> HttpT m a -> m a
runHttpT m h = runReaderT (unwrap h) m

-- | Given a 'Request' builder function, perform an actual HTTP request using the
-- respective method and return the response, fully consuming the response body
-- as a lazy 'ByteString'.
get, post, put, head, delete, options, trace, patch
    :: (MonadIO m, MonadHttp m)
    => (Request -> Request)
    -> m (Response (Maybe Lazy.ByteString))
get     f = httpLbs empty (method GET     . f)
post    f = httpLbs empty (method POST    . f)
put     f = httpLbs empty (method PUT     . f)
head    f = httpLbs empty (method HEAD    . f)
delete  f = httpLbs empty (method DELETE  . f)
options f = httpLbs empty (method OPTIONS . f)
trace   f = httpLbs empty (method TRACE   . f)
patch   f = httpLbs empty (method PATCH   . f)

get', post', put', head', delete', options', trace', patch'
    :: (MonadIO m, MonadHttp m)
    => Request
    -> (Request -> Request)
    -> m (Response (Maybe Lazy.ByteString))
get'     r f = httpLbs r (method GET     . f)
post'    r f = httpLbs r (method POST    . f)
put'     r f = httpLbs r (method PUT     . f)
head'    r f = httpLbs r (method HEAD    . f)
delete'  r f = httpLbs r (method DELETE  . f)
options' r f = httpLbs r (method OPTIONS . f)
trace'   r f = httpLbs r (method TRACE   . f)
patch'   r f = httpLbs r (method PATCH   . f)

httpLbs :: (MonadIO m, MonadHttp m)
        => Request
        -> (Request -> Request)
        -> m (Response (Maybe Lazy.ByteString))
httpLbs r f = http r f consumeBody

http :: (MonadIO m, MonadHttp m)
     => Request
     -> (Request -> Request)
     -> (Response BodyReader -> IO a)
     -> m a
http r f h = do
    m <- getManager
    liftIO $ withResponse (f r) m h

httpDebug :: (MonadIO m, MonadHttp m)
          => Debug
          -> Request
          -> (Request -> Request)
          -> (Response (Maybe Lazy.ByteString) -> IO a)
          -> m a
httpDebug debug r f h = do
    m <- getManager
    let rq = f r
    liftIO $ do
        if debug > Head
            then putStrLn (showRequest rq)
            else putStrLn (showRequest (rq { requestBody = RequestBodyLBS "" }))
        putStrLn "-"
        withResponse rq m $ consumeBody >=> \rsp -> do
            if debug > Head
                then putStrLn (showResponse rsp)
                else putStrLn (showResponse $ rsp { responseBody = ("" :: String) })
            putStrLn "--"
            h rsp

consumeBody :: Response BodyReader -> IO (Response (Maybe Lazy.ByteString))
consumeBody r = do
    chunks <- brConsume (responseBody r)
    let bdy = if null chunks
        then Nothing
        else Just (Lazy.fromChunks chunks)
    return $ r { responseBody = bdy }
