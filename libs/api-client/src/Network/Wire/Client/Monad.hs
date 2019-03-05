{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Wire.Client.Monad
    ( Server (..)
    , setServer
    , Client
    , MonadClient (..)
    , asyncClient
    , runClient
    , ClientException (..)
    ) where

import Imports hiding (log)
import Bilge
import Control.Concurrent.Async
import Network.HTTP.Types
import System.Logger.Class

import qualified System.Logger as Logger

data Env = Env
    { clientServer :: Server
    , clientLogger :: Logger
    }

newtype Client a = Client (ReaderT Env IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data Server = Server
    { serverHost    :: ByteString
    , serverPort    :: Word16
    , serverWsHost  :: Maybe ByteString
    , serverWsPort  :: Maybe Word16
    , serverSSL     :: Bool
    , serverManager :: Manager
    }

class (MonadHttp m, MonadLogger m, MonadIO m) => MonadClient m where
    getServer :: m Server
    getLogger :: m Logger

instance MonadHttp Client where
    getManager = Client $ asks (serverManager . clientServer)

instance MonadClient Client where
    getServer = Client $ asks clientServer
    getLogger = Client $ asks clientLogger

instance MonadLogger Client where
    log l m = getLogger >>= \lg -> Logger.log lg l m

setServer :: Server -> Request -> Request
setServer (Server h p _ _ s _) = host h . port p . (if s then secure else id)

asyncClient :: Client a -> Client (Async a)
asyncClient (Client c) = Client $ mapReaderT async c

runClient :: MonadIO m => Server -> Logger -> Client a -> m a
runClient s l (Client c) = liftIO $ runReaderT c (Env s l)

data ClientException
    = ErrorResponse Int Text Text
        -- ^ An error response from the API (code + label + message)
    | UnexpectedResponse Status ResponseHeaders Text
        -- ^ An unexpected response from the API.
    | ParseError Text
        -- ^ A response body could not be parsed.
    deriving (Show, Typeable)

instance Exception ClientException
