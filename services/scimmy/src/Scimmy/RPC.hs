{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


-- | General RPC utilities. TODO: refactor duplication with Brig.RPC, Galley.RPC, etc
module Scimmy.RPC where

import Bilge hiding (requestId)
import Bilge.Retry
import Bilge.RPC
import Control.Exception
import Control.Lens (view, makeLenses)
import Control.Monad (when)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Class()

import Control.Retry
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), checkResponse)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class hiding ((.=), name)

import qualified System.Logger            as Log
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as Text

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

remote :: ByteString -> Msg -> Msg
remote = field "remote"

decodeBody :: (FromJSON a, MonadThrow m) => Text -> Response (Maybe BL.ByteString) -> m a
decodeBody rm rs = case responseBody rs of
    Nothing -> throwM $ ParseException rm "Missing response body."
    Just bs -> decodeBytes rm bs

decodeBytes :: (FromJSON a, MonadThrow m) => Text -> BL.ByteString -> m a
decodeBytes ctx bs = case eitherDecode' bs of
    Left  e -> throwM $ ParseException ctx e
    Right a -> return a

expect :: [Status] -> Request -> Request
expect ss rq = rq { checkResponse = check }
  where
    check rq' rs = do
        let s   = responseStatus rs
            rs' = rs { responseBody = () }
        when (statusIsServerError s || s `notElem` ss) $
            throwM $ HttpExceptionRequest rq' (StatusCodeException rs' mempty)

-- | Failed to parse a response from another service.
data ParseException = ParseException
    { _parseExceptionRemote :: !Text
    , _parseExceptionMsg    :: String
    }

instance Show ParseException where
    show (ParseException r m) =
        "Failed to parse response from remote "
      ++ Text.unpack r ++ " with message: " ++ m

instance Exception ParseException

-------------------------------------------------------------------------------
-- App Monad
-- TODO: move App Monad to separate file
--
data Env = Env
    { _galley :: Request
    , _brig :: Request
    , _httpManager :: Manager
    , _applog        :: Logger
    , _requestId :: RequestId
    }

makeLenses ''Env

newtype AppT m a = AppT
    { unAppT :: ReaderT Env m a
    } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader Env
             )

type AppIO = AppT IO

instance Monad m => MonadHttp (AppT m) where
    getManager = view httpManager

instance Monad m => HasRequestId (AppT m) where
    getRequestId = view requestId

instance MonadBase IO AppIO where
    liftBase = liftIO

instance MonadIO m => MonadLogger (AppT m) where
    log l m = do
        g <- view applog
        r <- view requestId
        Log.log g l $ field "request" (unRequestId r) ~~ m


galleyRequest :: StdMethod
              -> (Request -> Request)
              -> AppIO (Response (Maybe BL.ByteString))
galleyRequest m r = do
    g <- view galley
    recovering x3 rpcHandlers $ const $
        rpc' "galley" g (method m . r)


brigRequest :: StdMethod
              -> (Request -> Request)
              -> AppIO (Response (Maybe BL.ByteString))
brigRequest m r = do
    g <- view brig
    recovering x3 rpcHandlers $ const $
        rpc' "brig" g (method m . r)
