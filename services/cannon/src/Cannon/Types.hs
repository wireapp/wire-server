{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cannon.Types
    ( Opts (..)
    , Env
    , mon
    , opts
    , applog
    , dict
    , env
    , logger
    , parseOptions
    , Cannon
    , mkEnv
    , runCannon
    , options
    , clients
    , monitor
    , wsenv
    ) where

import Bilge (Manager, RequestId (..), requestIdName)
import Bilge.RPC (HasRequestId (..))
import Cannon.Dict (Dict)
import Cannon.WS (Key, Websocket, Clock)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Metrics.Middleware
import Data.Word
import Network.Wai
import Options.Applicative
import System.Logger.Class hiding (info)
import System.Random.MWC (GenIO)

import qualified Cannon.WS     as WS
import qualified System.Logger as Logger

-----------------------------------------------------------------------------
-- Cannon monad

data Env = Env
    { mon     :: !Metrics
    , opts    :: !Opts
    , applog  :: !Logger
    , dict    :: !(Dict Key Websocket)
    , reqId   :: !RequestId
    , env     :: !WS.Env
    }

newtype Cannon a = Cannon
    { cannon :: ReaderT Env IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               )

instance MonadLogger Cannon where
    log l m = Cannon $ do
        g <- asks applog
        r <- field "request" . unRequestId <$> asks reqId
        liftIO $ Logger.log g l (r . m)

instance HasRequestId Cannon where
    getRequestId = Cannon $ asks reqId

mkEnv :: Metrics
      -> Opts
      -> Logger
      -> Dict Key Websocket
      -> Manager
      -> GenIO
      -> Clock
      -> Env
mkEnv m o l d p g t = Env m o l d mempty $
    WS.env (pack $ externalHost o) (port o) (gundeckHost o) (gundeckPort o) l p d g t

runCannon :: Env -> Cannon a -> Request -> IO a
runCannon e c r = let e' = e { reqId = lookupReqId r } in
    runReaderT (cannon c) e'

lookupReqId :: Request -> RequestId
lookupReqId = maybe mempty RequestId . lookup requestIdName . requestHeaders
{-# INLINE lookupReqId #-}

options :: Cannon Opts
options = Cannon $ asks opts

clients :: Cannon (Dict Key Websocket)
clients = Cannon $ asks dict

monitor :: Cannon Metrics
monitor = Cannon $ asks mon

wsenv :: Cannon WS.Env
wsenv = Cannon $ do
    e <- asks env
    r <- asks reqId
    return $ WS.setRequestId r e

logger :: Cannon Logger
logger = Cannon $ asks applog

-----------------------------------------------------------------------------
-- Command line options

data Opts = Opts
    { host         :: String
    , externalHost :: String
    , port         :: !Word16
    , gundeckHost  :: !ByteString
    , gundeckPort  :: !Word16
    } deriving (Eq, Show)

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Cannon - Websocket Push Service" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> metavar "HOSTNAME"
                <> help "host to listen on")

        <*> (strOption $
                long "external-host"
                <> metavar "HOSTNAME"
                <> help "host address to report to other services")

        <*> (option auto $
                long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "port to listen on")

        <*> (bytesOption $
                long "gundeck-host"
                <> metavar "HOSTNAME"
                <> help "Gundeck host")

        <*> (option auto $
                long "gundeck-port"
                <> metavar "PORT"
                <> help "Gundeck port")

    bytesOption :: Mod OptionFields String -> Parser ByteString
    bytesOption = fmap pack . strOption

